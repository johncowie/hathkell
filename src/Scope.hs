module Scope
( StateEither
, putLeft
, modifyS
, getEither
, runStateEither
, evalStateEither
, execStateEither
, get
) where

import qualified Control.Monad.State.Lazy as S
import qualified Control.Monad.Trans.Either as E

type StateEither e a = S.StateT e (E.EitherT String IO) a

runStateEither :: StateEither e a -> e -> IO (Either String (a, e))
runStateEither s = E.runEitherT . (S.runStateT s)

evalStateEither :: StateEither e a -> e -> IO (Either String a)
evalStateEither s = E.runEitherT . (S.evalStateT s)

execStateEither :: StateEither e a -> e -> IO (Either String e)
execStateEither s = E.runEitherT . (S.execStateT s)

putLeft :: String -> StateEither e a
putLeft s = S.StateT $ const (E.hoistEither (Left s))

modifyS :: (e -> e) -> StateEither e ()
modifyS = S.modify


-- FIXME might be able to do something funkier here
getEither :: (e -> Either String a) -> StateEither e a
getEither f = S.StateT $ \e -> case (f e) of
                                 (Right v) -> E.hoistEither (Right (v, e))
                                 (Left err) -> E.hoistEither (Left err)

-- getT :: (StateEither e e)
-- getT = S.getT

get :: (StateEither e e)
get = S.get
