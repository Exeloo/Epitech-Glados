import UnitTests.EvalUnitTests
import Test.HUnit (runTestTT)
import UnitTests.LaunchUnitTests
import UnitTests.UnitTestsParser (parserTestList)
import UnitTests.UnitTestAstEval
import UnitTests.UnitTestsSExprToAst (testListSExprToAst)
import UnitTests.UnitTestsSExprData (testListSExprData)

main :: IO ()
main = do
    _ <- runTestTT testlistEval
    _ <- runTestTT testlistLaunch
    _ <- runTestTT parserTestList
    _ <- runTestTT testlistAstEval
    _ <- runTestTT testListSExprToAst
    _ <- runTestTT testListSExprData
    return ()
