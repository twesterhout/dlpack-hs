module Main (main) where

import DLPack

main :: IO ()
main = do
  putStrLn "Hello world!"
  -- foldLoop1D 0 (< 10) (+ 1) (\_ i -> print i) ()
  foldN 0 [2, 2, 3] [8, 3, 1] (\() i -> print i) ()

--(\_ i -> print i) ()
