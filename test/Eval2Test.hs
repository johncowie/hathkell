module Eval2Test
(
tests
)
where

import qualified Eval2 as E
import qualified Global as G
import qualified Core as C
import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFPH
import qualified Control.Monad.State.Lazy as S
import qualified Scope as Scope
import Control.Monad.IO.Class (liftIO)
import Test.HUnit ((~:), (@=?), Test, assertBool, assertEqual, test)

data Case = ExpectSuccess String String String | ExpectFailure String String

success = ExpectSuccess
failure = ExpectFailure

evalCase f desc output input = (desc, output, input, f)

evalCases = [
               success "eval 1" "1"  "1"
             , success "eval sum" "4" "(+ 1 3)"
             , success "eval nested" "62" "(+ 2 4 (* 7 8))"
             , success "anonymous func" "9" "((fun (a) (* a a)) 3)"
             , success "minus" "-4" "(- 5 4 3 2)"
             , failure "scoping shouldn't leak" "((fun (a) (* a a)) 1) (* a a)"
             , success "basic if true" "7" "(if true 7 4)"
             , success "basic if false" "4" "(if false 7 4)"
             , success "basic def" "42" "(def a 6) (* a 7)"
             , failure "basic def reverse" "(* a 7) (def a 6)"
             , success "currying" "5" "(((fun (a) (fun (b) (+ a b))) 2) 3)"
             , success "def closure" "4" "(def f (fun (a) (fun () (* a a)))) ((f 2))"
             , success "closure with def in between" "24" "(def f (fun (a) (def c 4) (fun (b) (* c b a)))) ((f 2) 3)"
             , success "lower closure should win" "3" "(((fun (a) (fun (a) a)) 1) 3)"
             , success "equality" "false" "(= 2 3)"
             , success "equality" "true" "(= 2 (+ 1 1))"
            --  , success "gt" "true" "(> 2 1)"
            --  , success "gt" "false" "(> 4 4)"
            --  , success "gt" "false" "(> 5 7)"
             , success "lt" "true" "(< 1 2)"
             , success "lt" "false" "(< 2 1)"
             , success "lt" "false" "(< 0 0)"
             , success "and" "true" "(and true true true)"
             , success "and" "false" "(and false true false)"
             , success "and" "false" "(and false)"
             , success "return last" "9" "(+ 1 1) (+ 2 2) (* 3 3)"
             ]

isSuccess (Left _) = False
isSuccess (Right _) = True

showEither :: (Show a) => (Show b) => (Either a b) -> String
showEither (Left a) = show a
showEither (Right b) = show b

-- FIXME need to get core hathkell lib into scope
caseToTest :: Case -> IO ()
caseToTest (ExpectSuccess desc output input) = do
  actual <- Scope.evalStateEither (E.eval input) (C.globalContext G.globalScope)
  assertEqual desc output (showEither actual)
caseToTest (ExpectFailure desc input) = do
  actual <- Scope.evalStateEither (E.eval input) (C.globalContext G.globalScope)
  case actual of
    (Left s) -> assertEqual desc s s
    (Right s) -> assertEqual desc "this shouldn't have succeeded" (show s)

tests :: [TF.Test]
tests = concat . map (TFPH.hUnitTestToTests . test . caseToTest) $ evalCases
