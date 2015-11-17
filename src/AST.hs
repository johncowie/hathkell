module AST
where

import qualified Data.Map as Map
import Data.Maybe (maybe)
import Control.Monad (sequence, (>>=))
import Control.Applicative ((<*>), (<$>))
import Debug.Trace (trace)
import Text.Read (readMaybe)

data Symbol = Function String (Scope -> [Symbol] -> Either EvalError Symbol)
            | Literal Integer
            | SymbolName String
            | List [Symbol]

type ParseError = String
type EvalError = String
type EvalResult = String
type Scope = Map.Map String Symbol

plus :: Scope -> [Symbol] -> Either EvalError Symbol
plus scope a = do
  integers <- readIntegers scope a
  return $ Literal  . (foldr (+) 0) $ integers

minus :: Scope -> [Symbol] -> Either EvalError Symbol
minus scope [] = Left "- takes at least two args"
minus scope [a] = Left "- takes at least two args"
minus scope as = do
  (x:y:ys) <- readIntegers scope as
  return $ Literal $ foldl (-) (x - y) ys

mult :: Scope -> [Symbol] -> Either EvalError Symbol
mult scope [] = Left "* takes at least two args"
mult scope [a] = Left "* takes at least two args"
mult scope as = do
  xs <- readIntegers scope as
  return $ Literal $ foldl (*) 1 xs

instance Show Symbol where
  show (Function s f) = s
  show (Literal i) = show i
  show (SymbolName s) = s
  show (List s) = show s

symbolToInteger :: Symbol -> Either EvalError Integer
symbolToInteger (Literal i) = Right i
symbolToInteger (Function _ _) = Left "can't convert function to integer"

readInteger :: Scope -> Symbol -> Either EvalError Integer
readInteger scope a = (evalAST scope a) >>= symbolToInteger

readIntegers :: Scope -> [Symbol] -> Either EvalError [Integer]
readIntegers scope = sequence . map (readInteger scope)

-- anonFunction :: Scope -> [Symbol] -> Either EvalError Symbol
-- anonFunction scope [] = Left "anonymous function must have arg list"
-- anonFunction scope (argList:args) =

-- fun is called with an arglist

bindArgToScope :: Scope -> String -> Symbol -> Scope
bindArgToScope scope sym v = Map.insert sym v scope

bindArgsToScope :: Scope -> [Symbol] -> [Symbol] -> Either EvalError Scope
bindArgsToScope scope [] [] = Right scope
bindArgsToScope scope ((SymbolName s):ans) (ag:ags) =
  bindArgsToScope (bindArgToScope scope s ag) ans ags
bindArgsToScope scope (an:ans) (ag:ags) = Left "Args must be symbols"
bindArgsToScope _ _ _ = Left "Not the right number of args"

func :: [Symbol] -> [Symbol] -> Scope -> [Symbol] -> Either EvalError Symbol
func argNames body scope args = do
  newScope <- bindArgsToScope scope argNames args
  result <- sequence (map (evalAST newScope) body)
  return (last result)


createFunction :: Scope -> [Symbol] -> Either EvalError Symbol
createFunction scope [] = Left "anonymous function must have arg list"
createFunction scope ((List argList):body) = Right $ Function "anonymousFunction" (func argList body)

globalScope :: Map.Map String Symbol
globalScope = Map.fromList [  ("+", Function "+" plus)
                            , ("-", Function "-" minus)
                            , ("*", Function "*" mult)
                            , ("fun", Function "fun" createFunction)
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

addToLevel :: Symbol -> Int -> Symbol -> Symbol
addToLevel (List asts) 0 ast = List (asts ++ [ast])
addToLevel (List asts) n ast = List $ (dropLast asts) ++ [addToLevel (last asts) (n - 1) ast]

-- TODO, rewrite this from right to left, so can append to front above
-- also modify to use error instead of maybe
p :: Either ParseError (Symbol, Int) -> String -> Either ParseError (Symbol, Int)
p (Left "EOF") "(" = Right (List [], 0)
p (Right (ast, n)) "(" = Right ((addToLevel ast n (List [])), (n+1))
p (Right (ast, n)) ")"  = Right (ast, (n-1))
p (Right (ast, n)) x = do
  token <- evalTokenAST x
  return ((addToLevel ast n token), n)
p pair x = error $ (show x) ++ " " ++ (show pair)

pt :: [String] -> Either ParseError (Symbol, Int)
pt = foldl p (Left "EOF")

readAST :: String -> Either ParseError Symbol
readAST s = case (pt . tokens) s of  -- FIXME this case statement is broken
  Right (ast, -1) -> Right ast
  Right (ast, n) -> Left "Mismatched parentheses"
  Left err -> Left err

resolveSymbolName :: Scope -> Symbol -> Either EvalError Symbol
resolveSymbolName scope (SymbolName s) = lookupSymbol scope s
resolveSymbolName scope symbol = Right symbol

resolveFunction :: Scope -> Symbol -> Either EvalError Symbol
resolveFunction scope a = (evalAST scope a) >>= (resolveSymbolName scope)

execFunction :: Scope -> Symbol -> [Symbol] -> Either EvalError Symbol
execFunction scope (Function _ f) args = f scope args
execFunction scope (Literal _) _ = Left "Can't call a literal as a function"
execFunction scope (SymbolName s) _ = Left "Can't call unresolved symbol name as a function"

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

evalAST :: Map.Map String Symbol -> Symbol -> Either EvalError Symbol
evalAST localScope (List []) = Left "empty expression"
evalAST localScope (List (n:ns)) = do
  f <- resolveFunction localScope n
  execFunction localScope f ns
evalAST localScope v = resolveSymbolName localScope v

eval :: String -> Either EvalError Symbol
eval s = (readAST s) >>= (evalAST globalScope)

-- TODO
-- as well as global references, funtions need their own symbol scope - should that be part of the type?
--    how will arguments be loaded into function?  When function is applied, the arguments need to be bound to the scope of the function
--    e.g. (fun [a])
   -- 1) produces a function that takes one argument
  --  2) when the function is called, the argument is evaluated
    -- 3) the argument is then attached to the symbolName of the firstArgument in the function's scope map
      -- 4) when evaluating the ASTs that comprise the body of the function, this scope is passed to all of the evalASTs

  -- i)  make evalAST take a local scope
  -- ii) give function an optional argument list of symbols
  -- iii)
  -- iv) make a list function for producing a list
  -- v) need list Symbol type
