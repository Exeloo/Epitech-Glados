module UnitTests.LaunchUnitTests (testlistLaunch) where

import Test.HUnit
import Launch

testFileExist1 :: Test
testFileExist1 = TestCase $ do
    res <- fileExist ["notExist1", "notExist2"]
    assertEqual "no files exist" Nothing res

testFileExist2 :: Test
testFileExist2 = TestCase $ do
    res <- fileExist ["notExist1", ".gitignore"]
    assertEqual "file exist" (Just ".gitignore") res

testGetParamsLine1 :: Test
testGetParamsLine1 = TestCase (assertEqual "get first element" "params1" (getParamsLine ["params1", "-l"]))

testGetParamsLine2 :: Test
testGetParamsLine2 = TestCase (assertEqual "get last element" "params2" (getParamsLine ["-l", "params2"]))

testGetParamsLine3 :: Test
testGetParamsLine3 = TestCase (assertEqual "get empty string when the list is empty" [] (getParamsLine []))

testGetParamsLine4 :: Test
testGetParamsLine4 = TestCase (assertEqual "get empty string when the list only contains '-l' " [] (getParamsLine ["-l"]))

testLaunch1 :: Test
testLaunch1 = TestCase $ do
    res <- launch ["too", "many", "arguments"]
    assertEqual "too many arguments" False res

testLaunch2 :: Test
testLaunch2 = TestCase $ do
    res <- launch ["two", "arguments"]
    assertEqual "two arguments without '-l' " False res

testLaunch3 :: Test
testLaunch3 = TestCase $ do
    res <- launch ["README.md"]
    assertEqual "file not good" False res

testLaunch4 :: Test
testLaunch4 = TestCase $ do
    res <- launch ["(define foo 42)"]
    assertEqual "Simple line" True res

testlistLaunch :: Test
testlistLaunch = TestList [
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