module Eval2Test
(tests)
where

import qualified Eval2 as E
import qualified Global as G
import qualified Core as C
import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFPH
import qualified Control.Monad.State.Lazy as S
import Test.HUnit ((~:), (@=?), Test)

data Case = ExpectSuccess String String String | ExpectFailure String String

success = ExpectSuccess
failure = ExpectFailure

evalCase f desc output input = (desc, output, input, f)

evalCases = [  success "eval 1"
               "1"  "1"
             , success
               "eval sum" "4" "(+ 1 3)"
             , success "eval nested" "62" "(+ 2 4 (* 7 8))"
             , success "anonymous func" "9" "((fun (a) (* a a)) 3)"
             , success "minus" "-4" "(- 5 4 3 2)"
             , failure "scoping shouldn't leak" "((fun (a) (* a a)) 1) (* a a)"
             , success "basic if true" "7" "(if true 7 4)"
             , success "basic if false" "4" "(if false 7 4)"
             , success "basic def" "42" "(def a 6) (* a 7)"
             , failure "basic def reverse" "(* a 7) (def a 6)"
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
             ]

isSuccess (Left _) = False
isSuccess (Right _) = True

showSymbol = (fmap (show . fst))

-- FIXME need to get core hathkell lib into scope
caseToTest :: Case -> Test
caseToTest (ExpectSuccess desc output input) = desc ~: (Right output) @=? (showSymbol $ S.runStateT (E.eval input) (C.globalContext G.globalScope))
caseToTest (ExpectFailure desc input) =
  case showSymbol $ S.runStateT (E.eval input) (C.globalContext G.globalScope) of
    (Left s) -> desc ~: s @=? s
    (Right s) -> desc ~: "this shouldn't have succeeded" @=? s

tests :: [TF.Test]
tests = concat . map (TFPH.hUnitTestToTests . caseToTest) $ evalCases
