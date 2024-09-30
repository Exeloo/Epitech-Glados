import Test.HUnit
import UnitTestAstEval

main :: IO ()
main = do
    _ <- runTestTT testlistAstEval
    return ()
