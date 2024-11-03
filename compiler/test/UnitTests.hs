import Test.HUnit (runTestTT)
import UnitTests.LaunchUnitTests
import UnitTests.UnitTestsParser (parserTestList)
import UnitTests.UnitTestsSExprToAst (testListSExprToAst)
import UnitTests.UnitTestsSExprData (testListSExprData)

main :: IO ()
main = do
    _ <- runTestTT testlistLaunch
    _ <- runTestTT parserTestList
    _ <- runTestTT testListSExprToAst
    _ <- runTestTT testListSExprData
    return ()
