{-# LANGUAGE OverloadedStrings #-}

module DLPack.Context (
  importDLPack,
)
where

import Data.Map.Strict qualified as Map
import Language.C.Inline qualified as C
import Language.C.Inline.Context (Context (ctxTypesTable))
import Language.C.Types (CIdentifier, TypeSpecifier (..))
import Language.Haskell.TH (DecsQ, Q, TypeQ, lookupTypeName)
import Language.Haskell.TH.Syntax (Type (..))

importDLPack :: DecsQ
importDLPack =
  concat
    <$> sequence
      [ C.context =<< dlpackCxt
      , C.include "<dlpack.h>"
      ]

dlpackCxt :: Q C.Context
dlpackCxt = do
  typePairs <- Map.fromList <$> dlpackTypePairs
  pure $ C.baseCtx <> mempty{ctxTypesTable = typePairs}

dlpackTypePairs :: Q [(TypeSpecifier, TypeQ)]
dlpackTypePairs =
  optionals
    [ ("DLDevice", "DLDevice")
    , ("DLDataTypeCode", "DLDataTypeCode")
    , ("DLDataType", "DLDataType")
    , ("DLTensor", "DLTensor")
    , ("DLManagedTensor", "DLManagedTensor")
    ]
 where
  optional :: (CIdentifier, String) -> Q [(TypeSpecifier, TypeQ)]
  optional (cName, hsName) = do
    hsType <- case words hsName of
      [x] -> fmap ConT <$> lookupTypeName x
      -- TODO: generalize to multiple arguments
      [f, x] -> do
        con <- fmap ConT <$> lookupTypeName f
        arg <- fmap ConT <$> lookupTypeName x
        pure $ AppT <$> con <*> arg
      _ -> pure Nothing
    pure $ maybe [] (\x -> [(TypeName cName, pure x)]) hsType
  optionals :: [(CIdentifier, String)] -> Q [(TypeSpecifier, TypeQ)]
  optionals pairs = concat <$> mapM optional pairs
