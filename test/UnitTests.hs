import Test.HUnit ( runTestTT, Test(TestLabel, TestList) )
import LaunchUnitTests
import UnitTestsParser(parserTestList)

testList :: Test
testList = TestList [
    TestLabel "testFileExist1" testFileExist1,
    TestLabel "testFileExist2" testFileExist2,
    TestLabel "testGetParamsLine1" testGetParamsLine1,
    TestLabel "testGetParamsLine2" testGetParamsLine2,
    TestLabel "testGetParamsLine3" testGetParamsLine3,
    TestLabel "testGetParamsLine4" testGetParamsLine4,
    TestLabel "testLaunch1" testLaunch1,
    TestLabel "testLaunch2" testLaunch2,
    TestLabel "testLaunch3" testLaunch3,
    TestLabel "testLaunch4" testLaunch4
    ]

main :: IO ()
main = do
    _ <- runTestTT testList
    _ <- runTestTT parserTestList
    return ()
