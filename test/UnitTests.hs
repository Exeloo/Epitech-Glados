import EvalUnitTests
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
    TestLabel "testCheckElemList1" testCheckElemList1,
    TestLabel "testCheckElemList2" testCheckElemList2,
    TestLabel "testCheckElemList3" testCheckElemList3,
    TestLabel "testfindAssignation1" testfindAssignation1,
    TestLabel "testfindAssignation2" testfindAssignation2,
    TestLabel "testfindAssignation3" testfindAssignation3,
    TestLabel "testfindAssignation4" testfindAssignation4,
    TestLabel "testgetElemList1" testgetElemList1,
    TestLabel "testgetElemList2" testgetElemList2,
    TestLabel "testgetElemList3" testgetElemList3,
    TestLabel "testgetElemList4" testgetElemList4,
    TestLabel "testgetElemList5" testgetElemList5]

main :: IO ()
main = do
    _ <- runTestTT testList
    _ <- runTestTT parserTestList
    return ()
