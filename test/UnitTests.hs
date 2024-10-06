import UnitTests.EvalUnitTests
import Test.HUnit (runTestTT)
import UnitTests.LaunchUnitTests
import UnitTests.UnitTestsParser (parserTestList)
import UnitTests.UnitTestAstEval

main :: IO ()
main = do
    _ <- runTestTT testlistEval
    _ <- runTestTT testlistLaunch
    _ <- runTestTT parserTestList
    _ <- runTestTT testlistAstEval
    return ()
