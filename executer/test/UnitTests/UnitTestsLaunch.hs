module UnitTests.UnitTestsLaunch(testListLaunch) where

import Test.HUnit
import System.IO
import GHC.IO.Handle (hDuplicateTo, hDuplicate)
import System.Process.Internals (createPipe)
import Launch (launch)

captureStdout :: IO Bool -> IO String
captureStdout action = do
    (readEnd, writeEnd) <- createPipe
    originalStdOut <- hDuplicate stdout
    hDuplicateTo writeEnd stdout
    result <- hGetContents readEnd
    _ <- action
    hDuplicateTo originalStdOut stdout
    hClose writeEnd
    hClose readEnd
    return result

captureStderr :: IO Bool -> IO String
captureStderr action = do
    (readEnd, writeEnd) <- createPipe
    originalStderr <- hDuplicate stderr
    hDuplicateTo writeEnd stderr
    result <- hGetContents readEnd
    _ <- action
    hDuplicateTo originalStderr stderr
    hClose writeEnd
    hClose readEnd
    return result

testLaunchFactoriel :: Test
testLaunchFactoriel = TestCase $ do
    res <- captureStdout $ launch ["test/asm_test/factoriel"]
    assertEqual "Launch factoriel" "120" res

testWrongAsmAtParsing :: Test
testWrongAsmAtParsing = TestCase $ do
    res <- captureStderr $ launch ["test/asm_test/wrong_asm"]
    assertEqual "Launch wrong asm symbol" "Parse error: 1:7:\n  |\n1 | Push a5\n  |       ^\nunexpected '5'\nexpecting 'C', 'D', 'N', 'c', 'd', 'n', or white space\n" res

testWrongPath :: Test
testWrongPath = TestCase $ do
    res <- captureStderr $ launch ["lolilol"]
    assertEqual "Launch wrong path" "File lolilol do not exist\n" res

testInvalidArgs :: Test
testInvalidArgs = TestCase $ do
    res <- captureStderr $ launch ["lolilol", "b"]
    assertEqual "Launch wrong args" "Invalid args\n" res

testInvalidLabel :: Test
testInvalidLabel = TestCase $ do
    res <- captureStderr $ launch ["test/asm_test/wrong_label"]
    assertEqual "Launch wrong label" "Parse error: Jump label not found: abcd\n" res


testExecError :: Test
testExecError = TestCase $ do
    res <- captureStderr $ launch ["test/asm_test/exec_error"]
    assertEqual "Launch wrong label" "Exec error: Add need two numbers\n" res

testListLaunch :: Test
testListLaunch =
    TestList [
        TestLabel "testLaunchFactoriel" testLaunchFactoriel,
        TestLabel "testWrongAsmAtParsing" testWrongAsmAtParsing,
        TestLabel "testInvalidArgs" testInvalidArgs,
        TestLabel "testInvalidLabel" testInvalidLabel,
        TestLabel "testWrongPath" testWrongPath,
        TestLabel "testExecError" testExecError
    ]
