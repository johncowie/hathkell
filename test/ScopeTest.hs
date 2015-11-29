module ScopeTest
(tests)
where

import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFPH
import Test.HUnit ((~:), (@=?), (@?), Test)
import qualified Scope as S
import qualified Control.Monad.State.Lazy as ST
import qualified Control.Monad.Trans.Either as E
import Control.Monad (sequence)
import qualified Data.Map as M

canWrite = "Can write to state" ~: (Right (2, [3])) @=? (ST.runStateT m [])
  where m = do ST.modify ((:) 3)
               v <- pure 2
               return v :: S.StateEither [Int] Int

canError = "Can propogate an error and ignore later updates" ~: (Left "oh noes") @=? (ST.runStateT m [])
  where m = do ST.modify ((:) 3)
               S.putLeft "oh noes"
               ST.modify ((:) 4)
               return 2 :: S.StateEither [Int] Int

canJoin = "Can join a list of states" ~: (Right ([1, 2, 3, 4], [13, 12, 11, 10])) @=? (ST.runStateT states [])
  where m x i = do ST.modify ((:) i)
                   return x :: S.StateEither [Int] Int
        states = sequence (zipWith m [1..4] [10..13])

canJoinMap = "Can join map state" ~: (Right ([1..8], M.fromList [("a", 17)])) @=? (ST.runStateT states M.empty)
  where m x i = do ST.modify (M.insert "a" i)
                   return x :: S.StateEither (M.Map String Int) Int
        states = sequence (zipWith m [1..8] [10..])

tests :: [TF.Test]
tests = concat . map TFPH.hUnitTestToTests $ [ canWrite
                                              ,canError
                                              ,canJoin
                                              ,canJoinMap
                                              ]
