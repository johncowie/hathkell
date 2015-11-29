module Main where

import qualified Lib as L

main :: IO ()
main = do
  s <- getLine
  putStr $ L.main s

fromFile :: IO ()
fromFile = do
  s <- readFile "src-hth/fib.hth"
  putStrLn $ L.main s
