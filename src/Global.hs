module Global
(globalScope)
where

import qualified Eval2 as E
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Scope as S
import qualified Control.Monad.Trans.Either as TE
import Control.Monad.IO.Class (liftIO)
import Control.Monad ((>>=))
import Util (showTrace)
import Core (
              Scope
            , Symbol(..)
            , Val(..)
            , Fn
            , Bindings
            , asInteger
            , evalError
            , evalResult
            , function
            , intLiteral
            , boolLiteral
            , stringLiteral
            , symbolName
            , asBool
            , asSymbolName
            , addBinding
            , asString
            , wrapScope
            )

evalToInteger :: Symbol -> Scope Integer
evalToInteger a = (E.evalAST a) >>= asInteger

readIntegers :: [Symbol] -> Scope [Integer]
readIntegers = mapM evalToInteger

localBindings :: [String] -> [Symbol] -> Scope Bindings
localBindings argNames args
  | length argNames < length args = evalError "Too many args supplied"
  | length argNames > length args = evalError "Not enough args supplied"
  | otherwise = evalResult . Map.fromList . (zip argNames) $ args

func :: [String] -> [Symbol] -> [Symbol] -> Scope Symbol
func argNames body args = do
  evaluatedArgs <- (mapM E.evalAST args)
  localScope <- localBindings argNames evaluatedArgs
  (result:results) <- wrapScope localScope (mapM E.evalAST body) -- FIXME return last evaluated result for now -- introduce NIL later
  return result

evalArgNames :: [Symbol] -> Scope [String]
evalArgNames = mapM asSymbolName

createFunction :: Fn
createFunction [] = evalError "function must have arg list"
createFunction ((List argList):body) = do
  argNames <- evalArgNames argList
  return (function "anonymousFunction" (func argNames body))
createFunction [x] = evalError "function must have a body"
createFunction _ = evalError "first argument to function must be arg list"

plus :: Fn
plus a = do
  integers <- readIntegers a
  return . intLiteral  . (foldr (+) 0) $ integers

minus :: Fn
minus [] = evalError "- takes at least two args"
minus [a] = evalError "- takes at least two args"
minus as = do
  (x:y:ys) <- readIntegers as
  return . intLiteral $ foldl (-) (x - y) ys

mult :: Fn
mult [] = evalError "* takes at least two args"
mult [a] = evalError "* takes at least two args"
mult as = do
  xs <- readIntegers as
  return $ intLiteral $ foldl (*) 1 xs

ifFunc :: Fn
ifFunc [condition, success, failure] = do
  bAST <- E.evalAST condition -- FIXME what if there are new bindings?
  b <- asBool bAST
  if b
    then E.evalAST success
    else E.evalAST failure
ifFunc _ = evalError "Wrong number of args to if"

def :: Fn
def [symbolName, body] = do
  name <- asSymbolName symbolName
  v <- E.evalAST body
  addBinding (name, v)
  return symbolName
def _ = evalError "def takes two args, symbolName and value (or expression)"

equals :: Fn
equals [arg1, arg2] = do
  [a, b] <- mapM E.evalAST [arg1, arg2]
  return $ boolLiteral (a == b)
equals _ = evalError "= takes two args"

andBool :: Fn
andBool [] = evalError "'and' takes at least one arg"
andBool args = do
  symbols <- mapM E.evalAST args
  bools <- mapM asBool symbols
  return $ boolLiteral (and bools)

lt :: Fn
lt [arg1, arg2] = do
  [a, b] <- mapM E.evalAST [arg1, arg2]
  return $ boolLiteral (a < b)
lt _ = evalError "< takes two args"

readExp :: Fn
readExp [s] = E.evalAST s >>= asString >>= E.eval -- FIXME ast could generate new bindings
readExp args = evalError $ "read should take just one string argument" ++ show args

quote :: Fn
quote = return . stringLiteral . concat . (List.intersperse " ") . map show -- need to return string literal here

suck :: Fn
suck [s] = do
  fileName <- asString s
  content <- (liftIO $ readFile fileName)
  return (stringLiteral content)
suck args = evalError $ "suck should just take one argument"

blow :: Fn
blow [s, str] = do
  fileName <- asString s
  content <- asString str
  liftIO $ writeFile fileName content
  return s

globalScope :: Bindings
globalScope = Map.fromList [  ("+", function "+" plus)
                            , ("-", function "-" minus)
                            , ("*", function "*" mult)
                            , ("fun", function "fun" createFunction)
                            , ("if", function "if" ifFunc)
                            , ("def", function "def" def)
                            , ("=", function "=" equals)
                            , ("and", function "and" andBool)
                            , ("<", function "<" lt)
                            , ("read", function "read" readExp)
                            , ("quote", function "quote" quote)
                            , ("suck", function "suck" suck)
                            , ("blow", function "blow" blow)
                        ]
