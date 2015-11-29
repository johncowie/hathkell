module Lib
    (
    ) where

import qualified Core as C
import qualified Global as G
import qualified Eval2 as E
import qualified Scope as S
import qualified Data.Map as Map
import qualified System.Console.Haskeline as H
import qualified Control.Monad.IO.Class as IO

loadFiles :: C.Context -> [String] -> IO C.Context
loadFiles context [f] = do
  s <- readFile f
  r <- S.runStateEither (E.eval s) context
  case r of
    (Left err) -> do {putStrLn err; return context}
    (Right (v, newContext)) -> do {putStrLn (show v); return newContext}
loadFiles context (f:fs) = do
  s <- readFile f
  r <- S.execStateEither (E.eval s) context
  case r of
    (Left err) -> do {putStrLn err; return context}
    (Right st) -> loadFiles st fs

replR :: C.Context -> H.InputT IO ()
replR context = do
  line <- H.getInputLine "> "
  case line of
    Nothing -> return ()
    Just input -> do
                    r <- IO.liftIO $ S.runStateEither (E.eval input) context
                    case r of
                        (Left err) -> do { H.outputStrLn ("ERROR: " ++ err);
                                           replR context}
                        (Right (v, newContext)) -> do { H.outputStrLn ("=> " ++ (show v));
                                                        replR newContext}

repl = do
  context <- core
  H.runInputT H.defaultSettings (replR context)

core :: IO C.Context
core = loadFiles (C.globalContext G.globalScope) ["src-hth/core.hth"
                --                                , "src-hth/fib.hth"
                                                ]
