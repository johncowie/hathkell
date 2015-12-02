module Util
( firstRight
, applyFunctions
, showTrace
)
where

import Debug.Trace (trace)

applyFunctions :: [(a -> b)] -> a -> [b]
applyFunctions [] v = []
applyFunctions (f:fs) v = f v:applyFunctions fs v

firstRight :: a -> [Either a b] -> Either a b
firstRight defaultLeft [] = Left defaultLeft
firstRight defaultLeft ((Right x):xs) = Right x
firstRight defaultLeft (_:xs) = firstRight defaultLeft xs

showTrace :: (Show v) => String -> v -> v
showTrace s v = trace ("<" ++ s ++ " | " ++ (show v) ++ ">") v
