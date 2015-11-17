module AST
where

import qualified Data.Map as Map
import Data.Maybe (maybe)
import Control.Monad (sequence, (>>=))
import Control.Applicative ((<*>), (<$>))
import Debug.Trace (trace)
import Read(  Symbol(Function, Literal, SymbolName, List)
            , EvalError
            , ParseError
            , Scope
            , readAST
            )

type EvalResult = String

symbolToInteger :: Symbol -> Either EvalError Integer
symbolToInteger (Literal i) = Right i
symbolToInteger (Function _ _) = Left "can't convert function to integer"
symbolToInteger _ = Left "can't convert non literal type to integer"

readInteger :: Scope -> Symbol -> Either EvalError Integer
readInteger scope a = (evalAST scope a) >>= symbolToInteger

readIntegers :: Scope -> [Symbol] -> Either EvalError [Integer]
readIntegers scope = sequence . map (readInteger scope)

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

globalScope :: Map.Map String Symbol
globalScope = Map.fromList [  ("+", Function "+" plus)
                            , ("-", Function "-" minus)
                            , ("*", Function "*" mult)
                            , ("fun", Function "fun" createFunction)
                        ]

lookupSymbol :: Map.Map String Symbol -> String -> Either EvalError Symbol
lookupSymbol m s = maybe (Left "Couldn't resolve symbol ") Right (Map.lookup s m)

resolveSymbolName :: Scope -> Symbol -> Either EvalError Symbol
resolveSymbolName scope (SymbolName s) = lookupSymbol scope s
resolveSymbolName scope symbol = Right symbol

resolveFunction :: Scope -> Symbol -> Either EvalError Symbol
resolveFunction scope a = (evalAST scope a) >>= (resolveSymbolName scope)

execFunction :: Scope -> Symbol -> [Symbol] -> Either EvalError Symbol
execFunction scope (Function _ f) args = f scope args
execFunction scope (Literal _) _ = Left "Can't call a literal as a function"
execFunction scope (SymbolName s) _ = Left "Can't call unresolved symbol name as a function"
execFunction scope (List s) _ = Left "Can't call list as function"



evalAST :: Map.Map String Symbol -> Symbol -> Either EvalError Symbol
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
