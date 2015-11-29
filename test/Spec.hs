import Test.Framework (defaultMain, testGroup)
import qualified Eval2Test as Eval2
import qualified ScopeTest as Scope

main :: IO ()
main = defaultMain tests

tests = [ testGroup "evalTests" Eval2.tests
        , testGroup "scopeTests" Scope.tests]
