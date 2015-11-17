module Util
( firstRight
, applyFunctions
)
where

applyFunctions :: [(a -> b)] -> a -> [b]
applyFunctions [] v = []
applyFunctions (f:fs) v = f v:applyFunctions fs v

firstRight :: a -> [Either a b] -> Either a b
firstRight defaultLeft [] = Left defaultLeft
firstRight defaultLeft ((Right x):xs) = Right x
firstRight defaultLeft (_:xs) = firstRight defaultLeft xs
