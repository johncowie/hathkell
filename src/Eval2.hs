module Eval2
( evalAST
, eval
)
where

import qualified Control.Monad.State.Lazy as ST
import qualified Scope as S
import qualified Core as C
import qualified Data.Map as Map
import Read2 (readAST)
import Util (showTrace)

resolveSymbolName :: C.Symbol -> C.Scope C.Symbol  -- FIXME move this into core, or place that deals with scope
resolveSymbolName (C.Literal (C.SymbolRef s)) = C.getBinding s
resolveSymbolName symbol = C.evalResult symbol

execFunction :: C.Symbol -> [C.Symbol] -> C.Scope C.Symbol
execFunction s args = do
  (closures, f) <- (evalAST s) >>= C.asFunction
  if (Map.null closures)
    then (f args)
    else C.wrapScope closures (f args) -- if there are closures then add those to the execution context of the anon function

evalAST :: C.Symbol -> C.Scope C.Symbol
evalAST (C.List []) = C.evalError "empty expression"
evalAST (C.List (n:ns)) = execFunction n ns
evalAST v = resolveSymbolName v

eval :: String -> C.Scope C.Symbol
eval s = case (readAST s) of
  (Left err) -> C.evalError (show err)
  (Right syms) -> (fmap last) (mapM evalAST syms)
