{-# LANGUAGE DataKinds #-}

module Main (main) where

import DLPack
import GHC.Exts (IsList (..))

main :: IO ()
main = do
  putStrLn "Hello world!"

-- foldLoop1D 0 (< 10) (+ 1) (\_ i -> print i) ()
-- foldN 0 [2, 2, 3] [8, 3, 1] (\() i -> print i) ()
-- withDLTensor [2 :: Int, -8, 2, 3] $ \(t :: DLTensor 'DLCPU 1 Int) ->
--   print $ toList t

--(\_ i -> print i) ()
