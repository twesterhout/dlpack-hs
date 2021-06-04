-- |
-- Copyright: (c) 2021 Tom Westerhout
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
--
-- DLPack: Open In Memory Tensor Structure
module DLPack
  ( dlVersion,
    DLDeviceType (..),
    DLDevice (..),
    DLDataTypeCode (..),
    DLDataType (..),
    DLTensor (..),
    DLManagedTensor (..),
    IsDLDataType (..),
    IsDLTensor (..),
    tensorToFlatList,
    fold1,
    loop1,
    foldN,
    viaContiguousBuffer,
  )
where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad)
import Data.Functor.Identity
import Data.Int
import Data.Primitive (Prim)
import Data.Primitive.PrimArray
import qualified Data.Primitive.Ptr as P
import Data.Word
import Foreign.C.Types (CInt)
import Foreign.Marshal.Array (withArrayLen)
-- import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable
import Prelude hiding (init)

-- | Underlying DLPack version
dlVersion :: (Int, Int)
dlVersion = (0, 5)

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

data DLDevice = DLDevice
  { dlDeviceType :: !DLDeviceType,
    dlDeviceId :: {-# UNPACK #-} !Int
  }
  deriving stock (Read, Show, Eq)

instance Storable DLDevice where
  sizeOf _ = 8
  alignment _ = 4
  peek p =
    DLDevice
      <$> (toEnum . (fromIntegral :: CInt -> Int) <$> peekByteOff p 0)
      <*> ((fromIntegral :: CInt -> Int) <$> peekByteOff p 4)
  poke p x = do
    pokeByteOff p 0 . (fromIntegral :: Int -> CInt) . fromEnum . dlDeviceType $ x
    pokeByteOff p 4 (dlDeviceId x)

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
  fromEnum x = case x of
    DLInt -> 0
    DLUInt -> 1
    DLFloat -> 2
    DLOpaqueHandle -> 3
    DLBfloat -> 4
    DLComplex -> 5

data DLDataType = DLDataType
  { dlDataTypeCode :: !DLDataTypeCode,
    dlDataTypeBits :: {-# UNPACK #-} !Int,
    dlDataTypeLanes :: {-# UNPACK #-} !Int
  }
  deriving stock (Read, Show, Eq)

instance Storable DLDataType where
  sizeOf _ = 4
  alignment _ = 4
  peek p =
    DLDataType
      <$> (toEnum . (fromIntegral :: Word8 -> Int) <$> peekByteOff p 0)
      <*> ((fromIntegral :: Word8 -> Int) <$> peekByteOff p 1)
      <*> ((fromIntegral :: Word16 -> Int) <$> peekByteOff p 2)
  poke p x = do
    pokeByteOff p 0 . (fromIntegral :: Int -> Word8) . fromEnum . dlDataTypeCode $ x
    pokeByteOff p 1 . (fromIntegral :: Int -> Word8) . dlDataTypeBits $ x
    pokeByteOff p 2 . (fromIntegral :: Int -> Word16) . dlDataTypeLanes $ x

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
  alignment _ = 8
  peek p =
    DLTensor
      <$> peekByteOff p 0
      <*> peekByteOff p 8
      <*> ((fromIntegral :: CInt -> Int) <$> peekByteOff p 16)
      <*> peekByteOff p 20
      <*> peekByteOff p 24
      <*> peekByteOff p 32
      <*> peekByteOff p 40
  poke p x = do
    pokeByteOff p 0 (dlTensorData x)
    pokeByteOff p 8 (dlTensorDevice x)
    pokeByteOff p 16 . (fromIntegral :: Int -> CInt) . dlTensorNDim $ x
    pokeByteOff p 20 (dlTensorDType x)
    pokeByteOff p 24 (dlTensorShape x)
    pokeByteOff p 32 (dlTensorStrides x)
    pokeByteOff p 40 (dlTensorByteOffset x)

data DLManagedTensor = DLManagedTensor
  { dlManagedTensorTensor :: !DLTensor,
    dlManagedTensorManagerCxt :: {-# UNPACK #-} !(Ptr ()),
    dlManagedTensorDeleter :: {-# UNPACK #-} !(FunPtr (Ptr DLManagedTensor -> IO ()))
  }
  deriving stock (Show)

instance Storable DLManagedTensor where
  sizeOf _ = let x = x :: DLTensor in sizeOf x + 8 + 8
  alignment _ = let x = x :: DLTensor in alignment x
  peek p =
    DLManagedTensor
      <$> peekByteOff p 0
      <*> peekByteOff p 48
      <*> peekByteOff p 56
  poke p x = do
    pokeByteOff p 0 (dlManagedTensorTensor x)
    pokeByteOff p 48 (dlManagedTensorManagerCxt x)
    pokeByteOff p 56 (dlManagedTensorDeleter x)

class IsDLDataType a where
  dlDataTypeOf :: proxy a -> DLDataType

instance IsDLDataType Float where dlDataTypeOf _ = DLDataType DLFloat 64 1

instance IsDLDataType Double where dlDataTypeOf _ = DLDataType DLFloat 64 1

class Monad m => IsDLTensor m a where
  withDLTensor :: a -> (DLTensor -> m b) -> m b

fold1 :: Monad m => a -> (a -> Bool) -> (a -> a) -> (b -> a -> m b) -> b -> m b
fold1 start cond inc combine init = go start init
  where
    go !x !acc
      | cond x = acc `combine` x >>= go (inc x)
      | otherwise = return acc
{-# INLINE fold1 #-}

loop1 :: Monad m => a -> (a -> Bool) -> (a -> a) -> (a -> m ()) -> m ()
loop1 start cond inc f = fold1 start cond inc (\() x -> f x) ()
{-# INLINE loop1 #-}

foldN :: Monad m => Int -> [Int] -> [Int] -> (b -> Int -> m b) -> b -> m b
foldN = go
  where
    go :: Monad m => Int -> [Int] -> [Int] -> (b -> Int -> m b) -> b -> m b
    go _ [] [] _ acc = return acc
    go !start (!size : []) (!stride : []) combine acc =
      let combine' acc' !i = combine acc' (start + i)
       in fold1 0 (< size * stride) (+ stride) combine' acc
    go !start (!size : sizes) (!stride : strides) combine acc =
      let combine' acc' !i = go (start + i) sizes strides combine acc'
       in fold1 0 (< size * stride) (+ stride) combine' acc
    go _ _ _ _ _ = error "shape and strides have different lengths"

tensorToFlatList :: forall a. Prim a => DLTensor -> [a]
tensorToFlatList t = runIdentity $ foldN (dlTensorNDim t) shape strides combine []
  where
    buildList p = (\i -> fromIntegral $ P.indexOffPtr p i) <$> [0 .. dlTensorNDim t - 1]
    strides = buildList (dlTensorStrides t)
    shape = buildList (dlTensorShape t)
    combine xs !i =
      let !p = dlTensorData t `plusPtr` fromIntegral (dlTensorByteOffset t)
          !x = P.indexOffPtr (castPtr p) i
       in return $ x : xs

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

viaContiguousBuffer :: (IsDLDataType a, PrimMonad m) => Ptr a -> [Int] -> [Int] -> (DLTensor -> m b) -> m b
viaContiguousBuffer dataPtr shape strides action =
  withListN ndim (fromIntegral <$> shape) $ \shapePtr ->
    withListN ndim (fromIntegral <$> strides) $ \stridesPtr ->
      action
        DLTensor
          { dlTensorData = castPtr dataPtr,
            dlTensorDevice = DLDevice DLCPU 0,
            dlTensorNDim = ndim,
            dlTensorDType = dlDataTypeOf dataPtr,
            dlTensorShape = shapePtr,
            dlTensorStrides = stridesPtr,
            dlTensorByteOffset = 0
          }
  where
    !ndim = length shape

instance IsDLTensor IO [Double] where
  withDLTensor xs action = withArrayLen xs $ \size dataPtr ->
    viaContiguousBuffer dataPtr [size] [1] action

instance IsDLTensor IO [[Double]] where
  withDLTensor [] action = viaContiguousBuffer (nullPtr :: Ptr Double) [0, 0] [1, 1] action
  withDLTensor l@(x : _) action = do
    let n = length l
        m = length x
    withArrayLen (mconcat l) $ \size dataPtr -> do
      when (n * m /= size) $
        error $ "list has wrong shape: " <> show l
      viaContiguousBuffer dataPtr [n, m] [m, 1] action
