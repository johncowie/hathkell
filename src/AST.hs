module AST
where

import qualified Data.Map as Map
import Data.Maybe (maybe)
import Control.Monad (sequence, (>>=))
import Control.Applicative ((<*>), (<$>))
import Debug.Trace (trace)
import Read(  Symbol(Function, IntLiteral, SymbolName, List)
            , EvalError
            , ParseError
            , Scope
            , readAST
            , typeName
            , intLiteral
            , asInteger
            , asFunction
            , asBool
            )

type EvalResult = String

evalToInteger :: Scope -> Symbol -> Either EvalError Integer
evalToInteger scope a = (evalAST scope a) >>= asInteger

readIntegers :: Scope -> [Symbol] -> Either EvalError [Integer]
readIntegers scope = sequence . map (evalToInteger scope)

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
  evaluatedArgs <- sequence (map (evalAST scope) args)
  newScope <- bindArgsToScope scope argNames evaluatedArgs
  result <- sequence (map (evalAST newScope) body)
  return (last result)

createFunction :: Scope -> [Symbol] -> Either EvalError Symbol
createFunction scope [] = Left "function must have arg list"
createFunction scope ((List argList):body) = Right $ Function "anonymousFunction" (func argList body)
createFunction scope [x] = Left "function must have a body"
createFunction scope _ = Left "first argument to function must be arg list"

plus :: Scope -> [Symbol] -> Either EvalError Symbol
plus scope a = do
  integers <- readIntegers scope a
  return $ intLiteral  . (foldr (+) 0) $ integers

minus :: Scope -> [Symbol] -> Either EvalError Symbol
minus scope [] = Left "- takes at least two args"
minus scope [a] = Left "- takes at least two args"
minus scope as = do
  (x:y:ys) <- readIntegers scope as
  return $ intLiteral $ foldl (-) (x - y) ys

mult :: Scope -> [Symbol] -> Either EvalError Symbol
mult scope [] = Left "* takes at least two args"
mult scope [a] = Left "* takes at least two args"
mult scope as = do
  xs <- readIntegers scope as
  return $ intLiteral $ foldl (*) 1 xs

ifFunc :: Scope -> [Symbol] -> Either EvalError Symbol
ifFunc scope [condition, success, failure] = do
  bAST <- evalAST scope condition
  b <- asBool bAST
  if b
    then evalAST scope success
    else evalAST scope failure
ifFunc scope _ = Left "Wrong number of args to if"

globalScope :: Map.Map String Symbol
globalScope = Map.fromList [  ("+", Function "+" plus)
                            , ("-", Function "-" minus)
                            , ("*", Function "*" mult)
                            , ("fun", Function "fun" createFunction)
                            , ("if", Function "if" ifFunc)
                        ]

lookupSymbol :: Map.Map String Symbol -> String -> Either EvalError Symbol
lookupSymbol m s = maybe (Left "Couldn't resolve symbol ") Right (Map.lookup s m)

resolveSymbolName :: Scope -> Symbol -> Either EvalError Symbol
resolveSymbolName scope (SymbolName s) = lookupSymbol scope s
resolveSymbolName scope symbol = Right symbol

resolveFunction :: Scope -> Symbol -> Either EvalError Symbol
resolveFunction scope a = (evalAST scope a) >>= (resolveSymbolName scope)

execFunction :: Scope -> Symbol -> [Symbol] -> Either EvalError Symbol
execFunction scope s args = do
  f <- asFunction s
  f scope args

evalAST :: Scope -> Symbol -> Either EvalError Symbol
evalAST localScope (List []) = Left "empty expression"
evalAST localScope (List (n:ns)) = do
  f <- resolveFunction localScope n
  execFunction localScope f ns
evalAST localScope v = resolveSymbolName localScope v

eval :: String -> Either EvalError Symbol
eval s = (readAST s) >>= (evalAST globalScope)

-- TODO
-- split file into reader,  evaluator and core
-- split things up by passing in 'evaluator' to library functions
-- rename literal to int literal X
-- make boolean literal X
-- make 'if' X
-- figure out how to modify global bindings
