module UnitTests.LaunchUnitTests (testlistLaunch) where

import Test.HUnit
import System.IO
import Control.Exception (bracket_)
import GHC.IO.Handle (hDuplicateTo, hDuplicate)
import System.Process.Internals (createPipe)
import System.IO.Error (mkIOError, eofErrorType)
import GHC.IO.Exception (IOErrorType(..), IOErrorType(UserError))

import Launch

fileInput :: FilePath -> IO Bool -> IO Bool
fileInput filePath action = withFile filePath ReadMode
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

    bracket_ (return ()) (hDuplicateTo originalStdout stdout >> hClose writeEnd >> hClose readEnd) action

suppressOutput :: IO (Maybe Info) -> IO (Maybe Info)
suppressOutput action = do
    (readEnd, writeEnd) <- createPipe
    originalStdout <- hDuplicate stdout
    hDuplicateTo writeEnd stdout

    bracket_ (return ()) (hDuplicateTo originalStdout stdout >> hClose writeEnd >> hClose readEnd) action


paramsTooMany :: Test
paramsTooMany = TestCase $ do
    res <- suppressOutput $ errorHandling ["où", "est", "la", "pierre"]
    assertEqual "too many arguments" Nothing res

paramsEqualWithoutFlag :: Test
paramsEqualWithoutFlag = TestCase $ do
    res <- suppressOutput $ errorHandling ["-f", "file", "wrong"]
    assertEqual "3 arguments but not with flag -c" Nothing res

paramsErrorFlagC :: Test
paramsErrorFlagC = TestCase $ do
    res <- suppressOutput $ errorHandling ["-c", "wrong"]
    assertEqual "2 arguments with flag -c" Nothing res

paramsErrorFlagF :: Test
paramsErrorFlagF = TestCase $ do
    res <- suppressOutput $ errorHandling ["-f"]
    assertEqual "Forgot File" Nothing res

paramsErrorWithoutFlag :: Test
paramsErrorWithoutFlag = TestCase $ do
    res <- suppressOutput $ errorHandling ["file"]
    assertEqual "1 argument without any flag" Nothing res

paramsErrorFile :: Test
paramsErrorFile = TestCase $ do
    res <- suppressOutput $ errorHandling ["-f", "wrong"]
    assertEqual "File doesn't exist" Nothing res

paramsNotFindFile :: Test
paramsNotFindFile = TestCase $ assertEqual "doesn't have path in arguments" Nothing (getFilePath ["-f", "-c"])

paramsfileExist :: Test
paramsfileExist = TestCase $ do
    res <- fileExist "../example/test.glados"
    assertEqual "file exist" (Just "../example/test.glados") res

paramsErrorLaunch :: Test
paramsErrorLaunch = TestCase $ do
    res <- captureOutput $ launch ["-f", "wrong"]
    assertEqual "Error in launch" False res

paramsSuccess :: Test
paramsSuccess = TestCase $ do
    res <- captureOutput $ launch ["-f", "../example/test.glados"]
    assertEqual "Successful launch" True res

paramsArgWithoutFAndC :: Test
paramsArgWithoutFAndC = TestCase $ do
    res <- errorHandling []
    assertEqual "doesn't have any flags" (Just ((False, False), "")) res

paramsArgCWithoutF :: Test
paramsArgCWithoutF = TestCase $ do
    res <- errorHandling ["-c"]
    assertEqual "use flag c" (Just ((False, True), "")) res

paramsArgFWithoutC :: Test
paramsArgFWithoutC = TestCase $ do
    res <- errorHandling ["-f", "../example/test.glados"]
    assertEqual "use flag f" (Just ((True, False), "../example/test.glados")) res

paramsArgWithFAndC :: Test
paramsArgWithFAndC = TestCase $ do
    res <- errorHandling ["-f", "../example/test.glados", "-c"]
    assertEqual "use flag f and c" (Just ((True, True), "../example/test.glados")) res

launchFileStdout :: Test
launchFileStdout = TestCase $ do
    res <- captureOutput $ fileInput "../example/test.glados" (launch [])
    assertEqual "Launch with test file in standard output" True res

launchFileStdoutCompile :: Test
launchFileStdoutCompile = TestCase $ do
    res <- captureOutput $ fileInput "../example/test.glados" (launch ["-c"])
    assertEqual "Launch with test file in standard output and compile" True res

handleCtrlEOF :: Test
handleCtrlEOF = TestCase $ do
    res <- captureOutput $ handleCtrl (mkIOError eofErrorType "EOF Error" Nothing Nothing) "x = 5;" True
    assertEqual "handle EOF error but didn't have error in glados" True res

handleCtrlOther :: Test
handleCtrlOther = TestCase $ do
    res <- captureOutput $ handleCtrl (mkIOError UserError "Other Error" Nothing Nothing) "x = 42" False
    assertEqual "handle non-EOF error but having error in glados" False res

testlistLaunch :: Test
testlistLaunch = TestList [
    TestLabel "paramsTooMany" paramsTooMany,
    TestLabel "paramsEqualWithoutFlag" paramsEqualWithoutFlag,
    TestLabel "paramsErrorFlagC" paramsErrorFlagC,
    TestLabel "paramsErrorFlagF" paramsErrorFlagF,
    TestLabel "paramsErrorWithoutFlag" paramsErrorWithoutFlag,
    TestLabel "paramsErrorFile" paramsErrorFile,
    TestLabel "paramsNotFindFile" paramsNotFindFile,
    TestLabel "paramsfileExist" paramsfileExist,
    TestLabel "paramsErrorLaunch" paramsErrorLaunch,
    TestLabel "paramsSuccess" paramsSuccess,
    TestLabel "paramsArgWithoutFAndC" paramsArgWithoutFAndC,
    TestLabel "paramsArgCWithoutF" paramsArgCWithoutF,
    TestLabel "paramsArgFWithoutC" paramsArgFWithoutC,
    TestLabel "paramsArgWithFAndC" paramsArgWithFAndC,
    TestLabel "launchFileStdout" launchFileStdout,
    TestLabel "launchFileStdoutCompile" launchFileStdoutCompile,
    TestLabel "handleCtrlEOF" handleCtrlEOF,
    TestLabel "handleCtrlOther" handleCtrlOther
    ]