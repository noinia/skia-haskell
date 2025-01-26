module Main where

import Skia.Canvas.Raw(testRaw)
-- import Skia.Canvas

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  testRaw
