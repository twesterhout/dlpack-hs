{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: (c) 2021 Tom Westerhout
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
--
-- DLPack: Open In Memory Tensor Structure
module DLPack
  ( dlVersion,
    DLDeviceType (..),
    KnownDLDeviceType,
    -- natToInt,
    -- reifyDeviceType,
    DLDevice (..),
    DLDataTypeCode (..),
    DLDataType (..),
    DLTensor (..),
    -- DLManagedTensor (..),
    IsDLDataType (..),
    IsDLTensor (..),
    -- tensorToFlatList,
    -- fold1,
    -- loop1,
    -- foldN,
    viaContiguousBuffer,
    ContiguousVectorView (..),
    StridedVectorView (..),
    dlAsStridedVector,
    dlAsContiguousVector,
    dlExpectDType,
    dlExpectNDim,
  )
where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST
import Data.Complex
import Data.Functor.Identity
import Data.Int
import Data.Kind (Type)
import Data.Primitive (Prim)
import Data.Primitive.PrimArray
import qualified Data.Primitive.Ptr as P
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
-- import Foreign.Marshal.Utils (with)

import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable
import qualified GHC.Exts as GHC
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import Prelude hiding (init)

natToInt :: forall n. KnownNat n => Int
natToInt = fromIntegral $ GHC.TypeLits.natVal (Proxy @n)
{-# INLINE natToInt #-}

-- | Underlying DLPack version
dlVersion :: (Int, Int)
dlVersion = (0, 5)
{-# INLINE dlVersion #-}

-- | The device type in 'DLDevice'.
data DLDeviceType
  = -- | CPU device
    DLCPU
  | -- | CUDA GPU device
    DLCUDA
  | -- | Pinned CUDA CPU memory allocated by @cudaMallocHost@
    DLCUDAHost
  | -- | OpenCL device
    DLOpenCL
  | -- | Vulkan buffer for next generation graphics
    DLVulkan
  | -- | Metal for Apple GPU
    DLMetal
  | -- | Verilog simulator buffer
    DLVPI
  | -- | ROCm GPU device
    DLROCM
  | -- | Pinned ROCm CPU memory allocated by @hipMallocHost@
    DLROCMHost
  | -- | Reserved extension device type
    DLExtDev
  | -- | CUDA managed/unified memory allocated by @cudaMallocManaged@
    DLCUDAManaged
  deriving stock (Read, Show, Eq)

class KnownDLDeviceType (device :: DLDeviceType) where
  reifyDeviceType :: DLDeviceType

instance KnownDLDeviceType 'DLCPU where
  reifyDeviceType = DLCPU
  {-# INLINE reifyDeviceType #-}

instance KnownDLDeviceType 'DLCUDA where
  reifyDeviceType = DLCUDA
  {-# INLINE reifyDeviceType #-}

instance KnownDLDeviceType 'DLCUDAHost where
  reifyDeviceType = DLCUDAHost
  {-# INLINE reifyDeviceType #-}

instance KnownDLDeviceType 'DLOpenCL where
  reifyDeviceType = DLOpenCL
  {-# INLINE reifyDeviceType #-}

instance KnownDLDeviceType 'DLVulkan where
  reifyDeviceType = DLVulkan
  {-# INLINE reifyDeviceType #-}

instance KnownDLDeviceType 'DLMetal where
  reifyDeviceType = DLMetal
  {-# INLINE reifyDeviceType #-}

instance KnownDLDeviceType 'DLVPI where
  reifyDeviceType = DLVPI
  {-# INLINE reifyDeviceType #-}

instance KnownDLDeviceType 'DLROCM where
  reifyDeviceType = DLROCM
  {-# INLINE reifyDeviceType #-}

instance KnownDLDeviceType 'DLROCMHost where
  reifyDeviceType = DLROCMHost
  {-# INLINE reifyDeviceType #-}

instance KnownDLDeviceType 'DLExtDev where
  reifyDeviceType = DLExtDev
  {-# INLINE reifyDeviceType #-}

instance KnownDLDeviceType 'DLCUDAManaged where
  reifyDeviceType = DLCUDAManaged
  {-# INLINE reifyDeviceType #-}

instance Enum DLDeviceType where
  toEnum x = case x of
    1 -> DLCPU
    2 -> DLCUDA
    3 -> DLCUDAHost
    4 -> DLOpenCL
    7 -> DLVulkan
    8 -> DLMetal
    9 -> DLVPI
    10 -> DLROCM
    11 -> DLROCMHost
    12 -> DLExtDev
    13 -> DLCUDAManaged
    _ -> error $ "invalid DLDeviceType: " <> show x
  {-# INLINE toEnum #-}
  fromEnum x = case x of
    DLCPU -> 1
    DLCUDA -> 2
    DLCUDAHost -> 3
    DLOpenCL -> 4
    DLVulkan -> 7
    DLMetal -> 8
    DLVPI -> 9
    DLROCM -> 10
    DLROCMHost -> 11
    DLExtDev -> 12
    DLCUDAManaged -> 13
  {-# INLINE fromEnum #-}

data DLDevice = DLDevice
  { dlDeviceType :: !DLDeviceType,
    dlDeviceId :: {-# UNPACK #-} !Int
  }
  deriving stock (Read, Show, Eq)

instance Storable DLDevice where
  sizeOf _ = 8
  {-# INLINE sizeOf #-}
  alignment _ = 4
  {-# INLINE alignment #-}
  peek p =
    DLDevice
      <$> (toEnum . (fromIntegral :: CInt -> Int) <$> peekByteOff p 0)
      <*> ((fromIntegral :: CInt -> Int) <$> peekByteOff p 4)
  {-# INLINE peek #-}
  poke p x = do
    pokeByteOff p 0 . (fromIntegral :: Int -> CInt) . fromEnum . dlDeviceType $ x
    pokeByteOff p 4 (dlDeviceId x)
  {-# INLINE poke #-}

data DLDataTypeCode
  = DLInt
  | DLUInt
  | DLFloat
  | DLOpaqueHandle
  | DLBfloat
  | DLComplex
  deriving stock (Read, Show, Eq)

instance Enum DLDataTypeCode where
  toEnum x = case x of
    0 -> DLInt
    1 -> DLUInt
    2 -> DLFloat
    3 -> DLOpaqueHandle
    4 -> DLBfloat
    5 -> DLComplex
    _ -> error $ "invalid DLDataTypeCode: " <> show x
  {-# INLINE toEnum #-}
  fromEnum x = case x of
    DLInt -> 0
    DLUInt -> 1
    DLFloat -> 2
    DLOpaqueHandle -> 3
    DLBfloat -> 4
    DLComplex -> 5
  {-# INLINE fromEnum #-}

data DLDataType = DLDataType
  { dlDataTypeCode :: !DLDataTypeCode,
    dlDataTypeBits :: {-# UNPACK #-} !Int,
    dlDataTypeLanes :: {-# UNPACK #-} !Int
  }
  deriving stock (Read, Show, Eq)

instance Storable DLDataType where
  sizeOf _ = 4
  {-# INLINE sizeOf #-}
  alignment _ = 4
  {-# INLINE alignment #-}
  peek p =
    DLDataType
      <$> (toEnum . (fromIntegral :: Word8 -> Int) <$> peekByteOff p 0)
      <*> ((fromIntegral :: Word8 -> Int) <$> peekByteOff p 1)
      <*> ((fromIntegral :: Word16 -> Int) <$> peekByteOff p 2)
  {-# INLINE peek #-}
  poke p x = do
    pokeByteOff p 0 . (fromIntegral :: Int -> Word8) . fromEnum . dlDataTypeCode $ x
    pokeByteOff p 1 . (fromIntegral :: Int -> Word8) . dlDataTypeBits $ x
    pokeByteOff p 2 . (fromIntegral :: Int -> Word16) . dlDataTypeLanes $ x
  {-# INLINE poke #-}

data DLTensor = DLTensor
  { dlTensorData :: {-# UNPACK #-} !(Ptr ()),
    dlTensorDevice :: {-# UNPACK #-} !DLDevice,
    dlTensorNDim :: {-# UNPACK #-} !Int,
    dlTensorDType :: {-# UNPACK #-} !DLDataType,
    dlTensorShape :: {-# UNPACK #-} !(Ptr Int64),
    dlTensorStrides :: {-# UNPACK #-} !(Ptr Int64),
    dlTensorByteOffset :: {-# UNPACK #-} !Word64
  }
  deriving stock (Show, Eq)

instance Storable DLTensor where
  sizeOf _ = 8 + 8 + 4 + 4 + 8 + 8 + 8
  {-# INLINE sizeOf #-}
  alignment _ = 8
  {-# INLINE alignment #-}
  peek p =
    DLTensor
      <$> peekByteOff p 0
      <*> peekByteOff p 8
      <*> ((fromIntegral :: CInt -> Int) <$> peekByteOff p 16)
      <*> peekByteOff p 20
      <*> peekByteOff p 24
      <*> peekByteOff p 32
      <*> peekByteOff p 40
  {-# INLINE peek #-}
  poke p x = do
    pokeByteOff p 0 (dlTensorData x)
    pokeByteOff p 8 (dlTensorDevice x)
    pokeByteOff p 16 . (fromIntegral :: Int -> CInt) . dlTensorNDim $ x
    pokeByteOff p 20 (dlTensorDType x)
    pokeByteOff p 24 (dlTensorShape x)
    pokeByteOff p 32 (dlTensorStrides x)
    pokeByteOff p 40 (dlTensorByteOffset x)
  {-# INLINE poke #-}

class IsDLDataType a where
  dlDataTypeOf :: proxy a -> DLDataType

fromTag :: forall a proxy. Storable a => DLDataTypeCode -> proxy a -> DLDataType
fromTag tag _ =
  DLDataType {dlDataTypeCode = tag, dlDataTypeBits = (sizeOf element * 8), dlDataTypeLanes = 1}
  where
    element :: a
    element = undefined
{-# INLINE fromTag #-}

-- {{{ Integral

instance IsDLDataType CChar where
  dlDataTypeOf = fromTag DLInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CSChar where
  dlDataTypeOf = fromTag DLInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CUChar where
  dlDataTypeOf = fromTag DLUInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CShort where
  dlDataTypeOf = fromTag DLInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CUShort where
  dlDataTypeOf = fromTag DLUInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CInt where
  dlDataTypeOf = fromTag DLInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CUInt where
  dlDataTypeOf = fromTag DLUInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CLong where
  dlDataTypeOf = fromTag DLInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CULong where
  dlDataTypeOf = fromTag DLUInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CPtrdiff where
  dlDataTypeOf = fromTag DLInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CSize where
  dlDataTypeOf = fromTag DLUInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CLLong where
  dlDataTypeOf = fromTag DLInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CULLong where
  dlDataTypeOf = fromTag DLUInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CBool where
  dlDataTypeOf = fromTag DLUInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CIntPtr where
  dlDataTypeOf = fromTag DLInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CUIntPtr where
  dlDataTypeOf = fromTag DLUInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType Word8 where
  dlDataTypeOf = fromTag DLUInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType Word16 where
  dlDataTypeOf = fromTag DLUInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType Word32 where
  dlDataTypeOf = fromTag DLUInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType Word64 where
  dlDataTypeOf = fromTag DLUInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType Word where
  dlDataTypeOf = fromTag DLUInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType Int8 where
  dlDataTypeOf = fromTag DLInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType Int16 where
  dlDataTypeOf = fromTag DLInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType Int32 where
  dlDataTypeOf = fromTag DLInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType Int64 where
  dlDataTypeOf = fromTag DLInt
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType Int where
  dlDataTypeOf = fromTag DLInt
  {-# INLINE dlDataTypeOf #-}

-- }}}

-- {{{ Floating point

instance IsDLDataType CFloat where
  dlDataTypeOf = fromTag DLFloat
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType Float where
  dlDataTypeOf = fromTag DLFloat
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType CDouble where
  dlDataTypeOf = fromTag DLFloat
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType Double where
  dlDataTypeOf = fromTag DLFloat
  {-# INLINE dlDataTypeOf #-}

-- }}}

-- {{{ Complex

instance IsDLDataType (Complex CFloat) where
  dlDataTypeOf = fromTag DLComplex
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType (Complex Float) where
  dlDataTypeOf = fromTag DLComplex
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType (Complex CDouble) where
  dlDataTypeOf = fromTag DLComplex
  {-# INLINE dlDataTypeOf #-}

instance IsDLDataType (Complex Double) where
  dlDataTypeOf = fromTag DLComplex
  {-# INLINE dlDataTypeOf #-}

-- }}}

class (Monad m) => IsDLTensor m a where
  withDLTensor :: a -> (DLTensor -> m b) -> m b

data ContiguousVectorView a
  = ContiguousVectorView {-# UNPACK #-} !Int {-# UNPACK #-} !(Ptr a)

data StridedVectorView a
  = StridedVectorView {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !(Ptr a)

dlExpectDType :: forall a. IsDLDataType a => DLTensor -> Either Text ()
dlExpectDType t
  | expected == got = Right ()
  | otherwise =
    Left . T.pack $
      "expected DLTensor to be of type " <> show expected
        <> ", but it is of type "
        <> show got
  where
    !expected = dlDataTypeOf (Proxy @a)
    !got = dlTensorDType t
{-# INLINE dlExpectDType #-}

dlExpectDevice :: DLDevice -> DLTensor -> Either Text ()
dlExpectDevice expected t
  | expected == got = Right ()
  | otherwise =
    Left . T.pack $
      "expected the tensor to be on " <> show expected <> ", but it is on "
        <> show got
  where
    !got = dlTensorDevice t
{-# INLINE dlExpectDevice #-}

dlExpectNDim :: Int -> DLTensor -> Either Text ()
dlExpectNDim expected t
  | expected == got = Right ()
  | otherwise =
    Left . T.pack $
      "expected a " <> show expected <> "-dimensional tensor, but got a "
        <> show got
        <> "-dimensional one"
  where
    !got = dlTensorNDim t
{-# INLINE dlExpectNDim #-}

dlTensorLength :: DLTensor -> Int
dlTensorLength t
  | rank > 0 = fromIntegral $ go (P.indexOffPtr (dlTensorShape t) 0) 1
  | otherwise = 0
  where
    !rank = dlTensorNDim t
    go !acc !i
      | i < rank = go (acc * P.indexOffPtr (dlTensorShape t) i) (i + 1)
      | otherwise = acc
{-# INLINE dlTensorLength #-}

dlExpectShape :: PrimArray Int -> DLTensor -> Either Text ()
dlExpectShape expected t = do
  () <- dlExpectNDim rank t
  let go !i
        | i < rank =
          indexPrimArray expected i == fromIntegral (P.indexOffPtr (dlTensorShape t) i)
            && go (i + 1)
        | otherwise = True
  if go 0
    then Right ()
    else
      Left . T.pack $
        "expected a tensor of shape " <> show expected
          <> ", but got a tensor of shape "
          <> show shape
  where
    !rank = sizeofPrimArray expected
    shape = generatePrimArray (dlTensorNDim t) (\i -> P.indexOffPtr (dlTensorShape t) i)
{-# INLINE dlExpectShape #-}

dlAsStridedVector :: forall a. IsDLDataType a => DLTensor -> Either Text (StridedVectorView a)
dlAsStridedVector t = do
  () <- dlExpectDType @a t
  () <- dlExpectNDim 1 t
  let !size = fromIntegral $ P.indexOffPtr (dlTensorShape t) 0
      !stride = fromIntegral $ P.indexOffPtr (dlTensorStrides t) 0
  pure $ StridedVectorView size stride (castPtr (dlTensorData t))
{-# INLINE dlAsStridedVector #-}

dlAsContiguousVector :: forall a. IsDLDataType a => DLTensor -> Either Text (ContiguousVectorView a)
dlAsContiguousVector t = do
  (StridedVectorView size stride ptr) <- dlAsStridedVector t
  if stride == 1
    then Right $ ContiguousVectorView size ptr
    else Left . T.pack $ "expected a contiguous DLTensor, but got one with stride " <> show stride
{-# INLINE dlAsContiguousVector #-}

-- dlRowMajorStride :: DLTensor -> Maybe Int
-- dlRowMajorStride t = let !f = go 0 in f
--   where
--     go !i
--       | i < rank - 1 =
--         let !n₂ = P.indexOffPtr shape (i + 1)
--             !s₁ = P.indexOffPtr stride i
--             !s₂ = P.indexOffPtr stride (i + 1)
--          in if s₁ `div` s₂ == n₁ then go (i + 1) else False
--       | otherwise = True

-- data ShapeInfo = ShapeInfo {shapeSizes :: {-# UNPACK #-} !(PrimArray Int), shapeStrides :: {-# UNPACK #-} !(PrimArray Int)}
--   deriving stock (Show, Eq)

dlRowMajorStride :: DLTensor -> Maybe Int
dlRowMajorStride t
  | rank == 0 = Just 1
  | otherwise = fromIntegral <$> go (P.indexOffPtr stride 0) 1
  where
    !rank = dlTensorNDim t
    !shape = dlTensorShape t
    !stride = dlTensorStrides t
    go !s !i
      | i < rank =
        let !n = P.indexOffPtr shape i
            !s' = P.indexOffPtr stride i
         in if s `div` s' == n then go s' (i + 1) else Nothing
      | otherwise = Just s

--
-- isColumnMajorLike :: Int -> Ptr Int64 -> Ptr Int64 -> Bool
-- isColumnMajorLike !rank !shape !stride = let !f = go 0 in f
--   where
--     go !i
--       | i < rank - 1 =
--         let !n₁ = P.indexOffPtr shape i
--             !s₁ = P.indexOffPtr stride i
--             !s₂ = P.indexOffPtr stride (i + 1)
--          in if s₂ `div` s₁ == n₁ then go (i + 1) else False
--       | otherwise = True

-- toList1D :: forall a. Prim a => DLTensor 'DLCPU 1 a -> [a]
-- toList1D t = runST $ go [] (stride * (extent - 1))
--   where
--     !extent = dlExtent t 0
--     !stride = dlStride t 0
--     !p = dlTensorData t `plusPtr` fromIntegral (dlTensorByteOffset t)
--     go :: PrimMonad m => [a] -> Int -> m [a]
--     go acc !i
--       | i >= 0 = do !x <- P.readOffPtr p i; go (x : acc) (i - stride)
--       | otherwise = pure acc
--
-- chunksOf :: HasCallStack => Int -> [a] -> [[a]]
-- chunksOf k xs = undefined

-- tensorToFlatList :: forall a device rank. Prim a => DLTensor device rank a -> [a]
-- tensorToFlatList t = reverse $ runIdentity $ foldN 0 shape strides combine []
--   where
--     buildList p = (\i -> fromIntegral $ P.indexOffPtr p i) <$> [0 .. dlTensorNDim t - 1]
--     strides = buildList (dlTensorStrides t)
--     shape = buildList (dlTensorShape t)
--     combine xs !i =
--       let !p = dlTensorData t `plusPtr` fromIntegral (dlTensorByteOffset t)
--           !x = P.indexOffPtr (castPtr p) i
--        in return (x : xs)

withListN :: (Prim a, PrimMonad m) => Int -> [a] -> (Ptr a -> m b) -> m b
withListN n xs action = do
  array <- newAlignedPinnedPrimArray n
  let go i []
        | i == n = return ()
        | otherwise =
          error $
            "list is shorter than indicated: " <> show i <> " < " <> show n
      go !i (y : ys)
        | i < n = writePrimArray array i y >> go (i + 1) ys
        | otherwise =
          error $
            "list is shorter than indicated: " <> show (length xs) <> " > " <> show n
  go 0 xs
  action (mutablePrimArrayContents array)

viaContiguousBuffer ::
  (IsDLDataType a, PrimMonad m) =>
  Ptr a ->
  [Int] ->
  [Int] ->
  (DLTensor -> m b) ->
  m b
viaContiguousBuffer dataPtr shape strides action =
  withListN rank (fromIntegral <$> shape) $ \shapePtr ->
    withListN rank (fromIntegral <$> strides) $ \stridesPtr ->
      action
        DLTensor
          { dlTensorData = castPtr dataPtr,
            dlTensorDevice = DLDevice DLCPU 0,
            dlTensorNDim = rank,
            dlTensorDType = dlDataTypeOf dataPtr,
            dlTensorShape = shapePtr,
            dlTensorStrides = stridesPtr,
            dlTensorByteOffset = 0
          }
  where
    !rank = length shape
{-# INLINE viaContiguousBuffer #-}

class
  (Monad m, KnownNat d, IsDLDataType (ListElement a)) =>
  ListIsDLTensor (m :: Type -> Type) (d :: Nat) (a :: Type)
  where
  listWithDLTensor :: proxy d -> a -> (DLTensor -> m b) -> m b

listShape2 :: [[a]] -> (Int, Int)
listShape2 [] = (0, 0)
listShape2 xs@(x : _) = (length xs, length x)
{-# INLINE listShape2 #-}

listShape3 :: [[[a]]] -> (Int, Int, Int)
listShape3 [] = (0, 0, 0)
listShape3 xs@(x : _) = let (d₁, d₂) = listShape2 x in (length xs, d₁, d₂)
{-# INLINE listShape3 #-}

listShape4 :: [[[[a]]]] -> (Int, Int, Int, Int)
listShape4 [] = (0, 0, 0, 0)
listShape4 xs@(x : _) = let (d₁, d₂, d₃) = listShape3 x in (length xs, d₁, d₂, d₃)
{-# INLINE listShape4 #-}

listShape5 :: [[[[[a]]]]] -> (Int, Int, Int, Int, Int)
listShape5 [] = (0, 0, 0, 0, 0)
listShape5 xs@(x : _) = let (d₁, d₂, d₃, d₄) = listShape4 x in (length xs, d₁, d₂, d₃, d₄)
{-# INLINE listShape5 #-}

instance
  (PrimMonad m, ListElement [a] ~ a, IsDLDataType a, Prim a) =>
  ListIsDLTensor m 1 [a]
  where
  listWithDLTensor _ xs action = withListN d₀ xs $ \ptr ->
    viaContiguousBuffer ptr [d₀] [1] action
    where
      d₀ = length xs

instance
  (PrimMonad m, ListElement [[a]] ~ a, IsDLDataType a, Prim a) =>
  ListIsDLTensor m 2 [[a]]
  where
  listWithDLTensor _ xs action =
    withListN (d₀ * d₁) (mconcat xs) $ \ptr ->
      viaContiguousBuffer ptr [d₀, d₁] [d₁, 1] action
    where
      (d₀, d₁) = listShape2 xs

instance
  (PrimMonad m, ListElement [[[a]]] ~ a, IsDLDataType a, Prim a) =>
  ListIsDLTensor m 3 [[[a]]]
  where
  listWithDLTensor _ xs action =
    withListN (d₀ * d₁ * d₂) (mconcat . mconcat $ xs) $ \ptr ->
      viaContiguousBuffer ptr [d₀, d₁, d₂] [d₁ * d₂, d₂, 1] action
    where
      (d₀, d₁, d₂) = listShape3 xs

instance
  (PrimMonad m, ListElement [[[[a]]]] ~ a, IsDLDataType a, Prim a) =>
  ListIsDLTensor m 4 [[[[a]]]]
  where
  listWithDLTensor _ xs action =
    withListN (d₀ * d₁ * d₂ * d₃) (mconcat . mconcat . mconcat $ xs) $ \ptr ->
      viaContiguousBuffer ptr [d₀, d₁, d₂, d₃] [d₁ * d₂ * d₃, d₂ * d₃, d₃, 1] action
    where
      (d₀, d₁, d₂, d₃) = listShape4 xs

type family IsFinal a where
  IsFinal [a] = 'False
  IsFinal a = 'True

type family ListElement' a b where
  ListElement' a 'True = a
  ListElement' [a] 'False = ListElement' a (IsFinal a)

type ListElement (a :: Type) = ListElement' a (IsFinal a)

type family ListDimension' (n :: Nat) (a :: Type) where
  ListDimension' n [a] = ListDimension' (n + 1) a
  ListDimension' n a = n

type ListDimension (a :: Type) = ListDimension' 0 a

instance (Monad m, ListIsDLTensor m (ListDimension [t]) [t]) => IsDLTensor m [t] where
  withDLTensor = listWithDLTensor (Proxy :: Proxy (ListDimension [t]))

-- listShape2D :: [[a]] -> [Int]
-- listShape2D [] = [0, 0]
-- listShape2D xs@(x : _) = [length xs, length x]
--
-- listShape3D :: [[[a]]] -> [Int]
-- listShape3D [] = [0, 0, 0]
-- listShape3D xs@(x : _) = length xs : listShape2D x
--
-- listShape4D :: [[[a]]] -> [Int]
-- listShape4D [] = [0, 0, 0, 0]
-- listShape4D xs@(x : _) = length xs : listShape3D x
--
-- listShape5D :: [[[[a]]]] -> [Int]
-- listShape5D [] = [0, 0, 0, 0, 0]
-- listShape5D xs@(x : _) = length xs : listShape4D x

-- instance Prim a => GHC.IsList (DLTensor 'DLCPU 1 a) where
--   type Item (DLTensor 'DLCPU 1 a) = a
--   toList = toList1D
--   fromList _ = error "IsList instance of DLTensor does not implement toList"

-- instance Prim a => GHC.IsList (DLTensor 'DLCPU 2 a) where
--   type Item (DLTensor 'DLCPU 2 a) = [a]
--   toList t = case dlFlatten t of
--     Just flat -> chunksOf k . toList1D $ flat
--     Nothing -> error "IsList instance of DLTensor does not support weirdly strided tensors"
--     where
--       !k = dlExtent t 1
--   fromList _ = error "IsList instance of DLTensor does not implement toList"

-- instance Prim a => GHC.IsList (DLTensor 'DLCPU 3 a) where
--   type Item (DLTensor 'DLCPU 3 a) = [[a]]
--   toList t = case dlFlatten t of
--     Just flat -> chunksOf d₁ . chunksOf d₂ . toList1D $ flat
--     Nothing -> error "IsList instance of DLTensor does not support weirdly strided tensors"
--     where
--       !d₁ = dlExtent t 1
--       !d₂ = dlExtent t 2
--   fromList _ = error "IsList instance of DLTensor does not implement toList"

-- instance (IsDLDataType a, Storable a) => IsDLTensor IO 'DLCPU 1 a [a] where
--   withDLTensor xs action = withArrayLen xs $ \size dataPtr ->
--     viaContiguousBuffer dataPtr [size] [1] action
--   {-# INLINE withDLTensor #-}
--
-- instance (IsDLDataType a, Storable a) => IsDLTensor IO 'DLCPU 2 a [[a]] where
--   withDLTensor l action = do
--     let (!n, !m) = listShape2D l
--     withArrayLen (mconcat l) $ \size dataPtr -> do
--       when (n * m /= size) $ error "list has wrong shape"
--       viaContiguousBuffer dataPtr [n, m] [m, 1] action
--   {-# INLINE withDLTensor #-}
--
-- instance (IsDLDataType a, Storable a) => IsDLTensor IO 'DLCPU 3 a [[[a]]] where
--   withDLTensor l action = do
--     let (!d₁, !d₂, !d₃) = listShape3D l
--     withArrayLen (mconcat (mconcat l)) $ \size dataPtr -> do
--       when (d₁ * d₂ * d₃ /= size) $ error "list has wrong shape"
--       viaContiguousBuffer dataPtr [d₁, d₂, d₃] [d₂ * d₃, d₃, 1] action
--   {-# INLINE withDLTensor #-}
