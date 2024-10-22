module UnitTests.LaunchUnitTests (testlistLaunch) where

import Test.HUnit
import System.IO
import Control.Exception (bracket)
import GHC.IO.Handle (hDuplicateTo, hDuplicate)
import System.Process.Internals (createPipe)
import System.IO.Error (mkIOError, eofErrorType)
import GHC.IO.Exception (IOErrorType(..), IOErrorType(UserError))

import Launch

fileInput :: FilePath -> IO Bool -> IO Bool
fileInput filePath action = bracket
    (openFile filePath ReadMode)
    hClose
    (\fileHandle -> do
        originalStdin <- hDuplicate stdin
        hDuplicateTo fileHandle stdin
        result <- action
        hDuplicateTo originalStdin stdin
        return result)

captureOutput :: IO Bool -> IO Bool
captureOutput action = do
    (readEnd, writeEnd) <- createPipe
    originalStdout <- hDuplicate stdout
    hDuplicateTo writeEnd stdout

    result <- bracket (return ())
                      (const $ hDuplicateTo originalStdout stdout >> hClose writeEnd >> hClose readEnd)
                      (\_ -> action)
    return result


paramsFileNotExist :: Test
paramsFileNotExist = TestCase $ do
    res <- fileExist "notExist1"
    assertEqual "no files exist" Nothing res

paramsFileExist :: Test
paramsFileExist = TestCase $ do
    res <- fileExist ".gitignore"
    assertEqual "file exist" (Just ".gitignore") res

launchTooMany :: Test
launchTooMany = TestCase $ do
    res <- launch ["too", "many"]
    assertEqual "too many arguments" False res

launchFileNotGood :: Test
launchFileNotGood = TestCase $ do
    res <- captureOutput $ launch ["test/files_tests/lisp_test_error"]
    assertEqual "file not good" False res

launchFileGood :: Test
launchFileGood = TestCase $ do
    res <- captureOutput $ launch ["test/files_tests/lisp_test"]
    assertEqual "file good" True res

launchParamsLineGood :: Test
launchParamsLineGood = TestCase $ do
    res <- captureOutput $ launch ["(define foo 42)"]
    assertEqual "Simple line" True res

launchFileStdout :: Test
launchFileStdout = TestCase $ do
    res <-  captureOutput $ fileInput "test/files_tests/lisp_test" (launch [])
    assertEqual "Launch with lisp_test input" True res

launchFileStdoutError :: Test
launchFileStdoutError = TestCase $ do
    res <-  captureOutput $ fileInput "test/files_tests/lisp_test_error" (launch [])
    assertEqual "Launch with lisp_test_error input" False res

launchParamsLineInvalid :: Test
launchParamsLineInvalid = TestCase $ do
    res <- captureOutput $ launch ["(42)"]
    assertEqual "Simple line but Invalid lisp" False res

handleCtrlEOF :: Test
handleCtrlEOF = TestCase $ do
    res <- handleCtrl (mkIOError eofErrorType "EOF Error" Nothing Nothing) True
    assertEqual "handle EOF error but didn't have error in glados" True res

handleCtrlOther :: Test
handleCtrlOther = TestCase $ do
    res <- handleCtrl (mkIOError UserError "Other Error" Nothing Nothing) False
    assertEqual "handle non-EOF error but having error in glados" False res

testlistLaunch :: Test
testlistLaunch = TestList [
    TestLabel "paramsFileNotExist" paramsFileNotExist,
    TestLabel "paramsFileExist" paramsFileExist,
    TestLabel "launchTooMany" launchTooMany,
    TestLabel "launchFileNotGood" launchFileNotGood,
    TestLabel "launchFileGood" launchFileGood,
    TestLabel "launchParamsLineGood" launchParamsLineGood,
    TestLabel "launchFileStdout" launchFileStdout,
    TestLabel "launchFileStdoutError" launchFileStdoutError,
    TestLabel "launchParamsLineInvalid" launchParamsLineInvalid,
    TestLabel "handleCtrlEOF" handleCtrlEOF,
    TestLabel "handleCtrlOther" handleCtrlOther
    ]