module Main (main) where

import DLPack

main :: IO ()
main = do
  putStrLn ("Test suite is not implemented" :: String)
  let a :: [Double]
      a = [1, 2, 3, 4]
      b :: [[Double]]
      b =
        [ [1, 2, 3],
          [4, 5, 6]
        ]
  a' <- withDLTensor a $ \t ->
    return (tensorToFlatList @Double t)
  print a'
  b' <- withDLTensor b $ \t ->
    return (tensorToFlatList @Double t)
  print b'
