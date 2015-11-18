module Read
( Symbol(..)
, ParseError(..)
, EvalError(..)
, Scope(..)
, readAST
, typeName
, intLiteral
, asInteger
, asFunction
, asBool
)
where

import qualified Data.Map as Map
import Util (firstRight, applyFunctions)
import Text.Read (readMaybe)

type ParseError = String
type EvalError = String
data Symbol = Function String (Scope -> [Symbol] -> Either EvalError Symbol)
            | IntLiteral Integer
            | BooleanLiteral Bool
            | SymbolName String
            | List [Symbol]

type Scope = Map.Map String Symbol

instance Show Symbol where
  show (Function s f) = s
  show (IntLiteral i) = show i
  show (SymbolName s) = s
  show (List s) = show s
  show (BooleanLiteral s) = show s

intLiteral :: Integer -> Symbol
intLiteral = IntLiteral

boolLiteral :: Bool -> Symbol
boolLiteral = BooleanLiteral

typeName :: Symbol -> String
typeName (Function _ _) = "Function"
typeName (IntLiteral _) = "Integer"
typeName (SymbolName _) = "Symbol"
typeName (List _) = "List"
typeName (BooleanLiteral _) = "Bool"
-- typeName s = "Unknown"

asInteger :: Symbol -> Either EvalError Integer
asInteger (IntLiteral i) = Right i
asInteger s = Left ("Can't cast " ++ (typeName s) ++ " to Integer")

asFunction :: Symbol -> Either EvalError (Scope -> [Symbol] -> Either EvalError Symbol)
asFunction (Function n f) = Right f
asFunction s = Left ("Can't cast " ++ (typeName s) ++ " to Function")

asBool :: Symbol -> Either EvalError Bool
asBool (BooleanLiteral b) = Right b
asBool s = Left ("Can't cast " ++ (typeName s) ++ " to Bool")


--- reader stuff

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [x] = []
dropLast (x:xs) = x:dropLast xs

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

evalTokenAST ::  String -> Either ParseError Symbol
evalTokenAST s = firstRight ("Can't parse token " ++ s) .
                 applyFunctions [readIntLiteral,
                                 readBoolLiteral,
                                 readSymbolName] $ s

readEither :: (Read a) => String -> String -> Either EvalError a
readEither typeName s = maybe (Left errorMessage) Right (readMaybe s)
 where errorMessage = "can't parse " ++ s ++ " to type " ++ typeName

readIntLiteral :: String -> Either ParseError Symbol
readIntLiteral = (fmap IntLiteral) . (readEither "Integer")

readBoolLiteral :: String -> Either ParseError Symbol
readBoolLiteral "true" = Right (boolLiteral True)
readBoolLiteral "false" = Right (boolLiteral False)
readBoolLiteral s = Left ("can't parse " ++ s ++ " to type Bool")

readSymbolName :: String -> Either ParseError Symbol
readSymbolName = Right . SymbolName

readAST :: String -> Either ParseError Symbol
readAST s = case (pt . tokens) s of
  Right (ast, -1) -> Right ast
  Right (ast, n) -> Left "Mismatched parentheses"
  Left err -> Left err
