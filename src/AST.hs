module AST
where

import qualified Data.Map as Map
import Data.Maybe (maybe)
import Control.Monad (sequence, (>>=))
import Control.Applicative ((<*>), (<$>))
import Text.Regex (mkRegex, matchRegex)
import Text.Regex.Posix (getAllTextMatches, (=~))
import Debug.Trace (trace)
import Text.Read (readMaybe)

data AST = Expression [AST] | Value String deriving Show
data Symbol = Function String ([AST] -> Either EvalError Symbol) | Literal Integer | SymbolName String
type ParseError = String
type EvalError = String
type EvalResult = String

plus :: [AST] -> Either EvalError Symbol
plus a = do
  integers <- readIntegers a
  return $ Literal  . (foldr (+) 0) $ integers

minus :: [AST] -> Either EvalError Symbol
minus [] = Left "- takes at least two args"
minus [a] = Left "- takes at least two args"
minus as = do
  (x:y:ys) <- readIntegers as
  return $ Literal $ foldl (-) (x - y) ys

mult :: [AST] -> Either EvalError Symbol
mult [] = Left "* takes at least two args"
mult [a] = Left "* takes at least two args"
mult as = do
  xs <- readIntegers as
  return $ Literal $ foldl (*) 1 xs

instance Show Symbol where
  show (Function s f) = s
  show (Literal i) = show i
  show (SymbolName s) = s

symbolToInteger :: Symbol -> Either EvalError Integer
symbolToInteger (Literal i) = Right i
symbolToInteger (Function _ _) = Left "can't convert function to integer"
symbolToInteger (SymbolName _) = Left "can't convert symbol to integer"

readInteger :: AST -> Either EvalError Integer
readInteger a = (evalAST a) >>= symbolToInteger

readIntegers :: [AST] -> Either EvalError [Integer]
readIntegers = sequence . map readInteger

anonFunction :: [AST] -> Either EvalError Symbol
anonFunction = fmap last . sequence . map evalAST

storeFunction :: [AST] -> Either EvalError Symbol
storeFunction args = Right (Function "an anonymous function" anonFunction)

symbols :: Map.Map String Symbol
symbols = Map.fromList [  ("+", Function "+" plus)
                        , ("-", Function "-" minus)
                        , ("*", Function "*" mult)
                        , ("fun", Function "fun" storeFunction)
                        ]

lookupSymbol :: Map.Map String Symbol -> String -> Either EvalError Symbol
lookupSymbol m s = maybe (Left "Couldn't resolve symbol ") Right (Map.lookup s m)

whitespace :: Char -> Bool
whitespace ' ' = True
whitespace '\n' = True
whitespace '\t' = True
whitespace _ = False

replace :: Char -> String -> String -> String
replace _ _ [] = []
replace o replacement (c:cs)
  | o == c = replacement ++ (replace o replacement cs)
  | otherwise = c:(replace o replacement cs)

splitOn :: (Char -> Bool) -> String -> [String]
splitOn _ [] = []
splitOn p xs =
  case firstWord of
    "" -> []
    _  -> firstWord:splitOn p rest
  where (_,trimmed) = break (not . p) xs
        (firstWord,rest) = break p trimmed


tokens = (splitOn whitespace) .
         (replace ')' " ) ") .
         (replace '(' " ( ")

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [x] = []
dropLast (x:xs) = x:dropLast xs

addToLevel :: AST -> Int -> AST -> AST
addToLevel (Expression asts) 0 ast = Expression (asts ++ [ast])
addToLevel (Expression asts) n ast = Expression $ (dropLast asts) ++ [addToLevel (last asts) (n - 1) ast]

-- TODO, rewrite this from right to left, so can append to front above
-- also modify to use error instead of maybe
p :: (Maybe AST, Int) -> String -> (Maybe AST, Int)
p (Nothing, n) "(" = (Just (Expression []), (n+1))
p ((Just ast), n) "(" = (Just (addToLevel ast n (Expression [])), (n+1))
p ((Just ast), n) ")"  = (Just ast, (n-1))
p ((Just ast), n) x = (Just (addToLevel ast n (Value x)), n)
p pair x = error $ (show x) ++ " " ++ (show pair)

pt :: [String] -> (Maybe AST, Int)
pt = foldl p (Nothing, -1)

readAST :: String -> Either ParseError AST
readAST s = case (pt . tokens) s of
  (Just ast, -1) -> Right ast
  (Just ast, n) -> Left "Mismatched parentheses"
  otherwise -> Left "Reached EOF with nothing to parse"

resolveSymbolName :: Symbol -> Either EvalError Symbol
resolveSymbolName (SymbolName s) = lookupSymbol symbols s
resolveSymbolName symbol = Right symbol

evalFunction :: AST -> Either EvalError Symbol
evalFunction a = (evalAST a) >>= resolveSymbolName

execFunction :: Symbol -> [AST] -> Either EvalError Symbol
execFunction (Function _ f) args = f args
execFunction (Literal _) _ = Left "Can't call a literal as a function"
execFunction (SymbolName s) _ = Left "Can't call unresolved symbol name as a function"

readEither :: (Read a) => String -> String -> Either EvalError a
readEither typeName s = maybe (Left errorMessage) Right (readMaybe s)
  where errorMessage = "can't parse " ++ s ++ " to type " ++ typeName

evalIntLiteral :: String -> Either EvalError Symbol
evalIntLiteral = (fmap Literal) . (readEither "Integer")

evalSymbolName :: String -> Either EvalError Symbol
evalSymbolName = Right . SymbolName

applyFunctions :: [(a -> b)] -> a -> [b]
applyFunctions [] v = []
applyFunctions (f:fs) v = f v:applyFunctions fs v

firstRight :: a -> [Either a b] -> Either a b
firstRight defaultLeft [] = Left defaultLeft
firstRight defaultLeft ((Right x):xs) = Right x
firstRight defaultLeft (_:xs) = firstRight defaultLeft xs


evalTokenAST ::  String -> Either EvalError Symbol
evalTokenAST s = firstRight ("Can't parse token " ++ s) .
                 applyFunctions [evalIntLiteral, evalSymbolName] $ s

evalAST :: AST -> Either EvalError Symbol
evalAST (Expression []) = Left "empty expression"
evalAST (Expression (n:ns)) = do
  f <- evalFunction n
  execFunction f ns
evalAST (Value s) = evalTokenAST s

eval :: String -> Either EvalError Symbol
eval s = (readAST s) >>= evalAST
