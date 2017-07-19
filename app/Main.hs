module Main where

import qualified Lib as L

main :: IO ()
main = error "blah"
-- main = do
  -- s <- getLine
  -- putStr $ L.core

-- fromFile :: IO ()
-- fromFile = do
--   s <- readFile "src-hth/fib.hth"
--   putStrLn $ L.core s
