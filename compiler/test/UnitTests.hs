import Test.HUnit (runTestTT)
import UnitTests.LaunchUnitTests
import UnitTests.UnitTestsSExprToAst (testListSExprToAst)
import UnitTests.UnitTestsSExprData (testListSExprData)
import UnitTests.UnitTestsBytecode (testListBytecode)

main :: IO ()
main = do
    _ <- runTestTT testlistLaunch
    _ <- runTestTT testListSExprToAst
    _ <- runTestTT testListSExprData
    _ <- runTestTT testListBytecode
    return ()
