module Core
( Val(SymbolRef, StringLiteral)
, Symbol(List, Literal)
, Fn
, Scope
, Bindings
, Closures
, Context
, listExpression
, intLiteral
, noneLiteral
, symbolName
, boolLiteral
, stringLiteral
, function
, asInteger
, asFunction
, asBool
, asString
, asSymbolName
, typeName
, evalError
, evalResult
, addBinding
, addLocalBinding
, mostLocalScope
, getBinding
-- , tryGetBinding
, wrapScope
, globalContext
, anonFunction
)
where

import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Debug.Trace (trace)
import qualified Data.Char as Char
import qualified Data.Ord as Ord
import Scope (StateEither, putLeft, modifyS, getEither, get)
import qualified Control.Monad.State.Lazy as ST

type ParseError = String
type EvalError = String
type Fn =  [Symbol] -> (Scope Symbol)
data Val =        StringLiteral String
                | BooleanLiteral Bool
                | IntLiteral Integer
                | SymbolRef String
                | None

data Symbol = Function String Closures Fn
            -- | AnonFunction Closures Fn
            | List [Symbol]
            | Literal Val

type Bindings = Map.Map String Symbol
type Closures = Bindings
type GlobalBindings = Bindings

data Context = LocalContext Context Bindings | GlobalContext Bindings deriving Show

type Scope a = StateEither Context a

globalContext m = GlobalContext m

addLocalScope :: Bindings -> Context -> Context
addLocalScope bindings parentContext = LocalContext parentContext bindings

removeLocalScope :: Context -> Context
removeLocalScope (LocalContext parentContext bindings) = parentContext

mostLocalScope :: Context -> Bindings
mostLocalScope (GlobalContext m) = Map.empty
mostLocalScope (LocalContext p bindings) = bindings

wrapScope :: Bindings -> Scope a -> Scope a
wrapScope bindings intermediate = do
  modifyS (addLocalScope bindings)
  v <- intermediate
  modifyS removeLocalScope
  return v

-- FIXME can create an abstraction for upsert and find, that navigates through these layers?
upsertBinding :: (String, Symbol) -> Context -> Context
upsertBinding (s, v) (LocalContext p b) = Maybe.maybe
                                          (LocalContext (upsertBinding (s, v) p) b)
                                          (const (LocalContext p (Map.insert s v b)))
                                          (Map.lookup s b)
upsertBinding (s, v) (GlobalContext b) = GlobalContext (Map.insert s v b)

insertLocalBinding :: (String, Symbol) -> Context -> Context
insertLocalBinding (s, v) (LocalContext p b) = (LocalContext p (Map.insert s v b))
insertLocalBinding (s, v) (GlobalContext b) = (GlobalContext (Map.insert s v b))

addBinding :: (String, Symbol) -> Scope ()
addBinding binding = modifyS (upsertBinding binding)

addLocalBinding :: (String, Symbol) -> Scope ()
addLocalBinding binding = modifyS (insertLocalBinding binding)

findBinding :: String -> Context -> Either String Symbol
findBinding s (LocalContext p b) = Maybe.maybe (findBinding s p) Right (Map.lookup s b)
findBinding s (GlobalContext b) = Maybe.maybe (Left ("Can't find symbol " ++ s ++ " in scope.")) Right (Map.lookup s b)

-- tryFindBinding :: String -> Context -> Symbol
-- tryFindBinding s (LocalContext p b) = Maybe.maybe (tryFindBinding s p) id (Map.lookup s b)
-- tryFindBinding s (GlobalContext b) = Maybe.maybe (symbolName s) id (Map.lookup s b)

getBinding :: String -> Scope Symbol
getBinding s = getEither (findBinding s)

-- tryGetBinding :: String -> Scope Symbol
-- tryGetBinding s = get (tryFindBinding s)

addBindings :: [(String, Symbol)] -> Scope [()]
addBindings bindings = sequence (map addBinding bindings)

evalResult :: a -> Scope a
evalResult = pure

evalError :: String -> Scope a
evalError = putLeft

instance Show Val where
  show (IntLiteral i) = show i
  show (SymbolRef s) = s
  show (StringLiteral s) = "\"" ++ s ++ "\""
  show (BooleanLiteral b) = map Char.toLower (show b)
  show None = "none"

instance Eq Val where
  (IntLiteral a) == (IntLiteral b) = a == b
  (SymbolRef a) == (SymbolRef b) = a == b
  (BooleanLiteral a) == (BooleanLiteral b) = a == b
  (StringLiteral a) == (StringLiteral b) = a == b

instance Ord Val where
  compare (IntLiteral a) (IntLiteral b)         = compare a b
  compare (SymbolRef a) (SymbolRef b)         = compare a b
  compare (BooleanLiteral a) (BooleanLiteral b) = compare a b
  compare (StringLiteral a) (StringLiteral b)   = compare a b
   -- FIXME should this somehow be an error?  maybe have function called asComparable

instance Show Symbol where
  show (Function s c f) = s
  -- show (AnonFunction b f) = "AnonymousFunction"
  show (Literal l) = show l
  show (List s) = "(" ++ (concat (List.intersperse " " (map show s)))  ++ ")"

instance Eq Symbol where
  (Literal a) == (Literal b) = a == b
  _ == _ = False

instance Ord Symbol where
  compare (Literal i1) (Literal i2) = compare i1 i2
  compare _ _ = Ord.LT

noneLiteral :: Symbol
noneLiteral = Literal None

intLiteral :: Integer -> Symbol
intLiteral = Literal . IntLiteral

boolLiteral :: Bool -> Symbol
boolLiteral = Literal . BooleanLiteral

stringLiteral :: String -> Symbol
stringLiteral = Literal . StringLiteral

function :: String -> Fn -> Symbol
function s f = Function s Map.empty f

anonFunction :: String -> Closures -> Fn -> Symbol
anonFunction = Function

listExpression :: [Symbol] -> Symbol
listExpression = List

symbolName = Literal . SymbolRef

typeName :: Symbol -> String
typeName (Function _ _ _) = "Function"
-- typeName (AnonFunction _ _) = "AnonFunction"
typeName (Literal (IntLiteral _)) = "Integer"
typeName (Literal (SymbolRef _)) = "Symbol"
typeName (List _) = "List"
typeName (Literal (BooleanLiteral _)) = "Bool"
typeName (Literal (StringLiteral _)) = "String"
typeName (Literal None) = "None"
-- typeName s = "Unknown"

castError requiredType s = evalError ("Can't cast " ++ (typeName s) ++ " " ++ (show s) ++ " to " ++ requiredType)

asInteger :: Symbol -> Scope Integer
asInteger (Literal (IntLiteral i)) = evalResult i
asInteger other = castError "Integer" other

asString :: Symbol -> Scope String
asString (Literal (StringLiteral s)) = evalResult s
asString other = castError "String" other

asFunction :: Symbol -> Scope (Closures, Fn)
asFunction (Function n c f) = evalResult (c, f)
asFunction other = castError "Function" other

asBool :: Symbol -> Scope Bool
asBool (Literal (BooleanLiteral b)) = evalResult b
asBool other = castError "Bool" other

asSymbolName :: Symbol -> Scope String
asSymbolName (Literal (SymbolRef s)) = evalResult s
asSymbolName other = castError "SymbolRef" other
