module UnitTests.UnitTestsSExprData(testListSExprData) where

import Test.HUnit
import SExprData

testShowSysCall :: Test
testShowSysCall = TestCase $ assertEqual "test show a syscall" ("SAdd") (show (SAdd))
testEqSysCall :: Test
testEqSysCall = TestCase $ assertEqual "test eq a syscall" (True) ((SAdd) == (SAdd))

testShowSValue :: Test
testShowSValue = TestCase $ assertEqual "test show a SValue" ("SInt 5") (show (SInt 5))
testEqSValue :: Test
testEqSValue= TestCase $ assertEqual "test eq a SValue" (True) ((SInt 5) == (SInt 5))

testShowSInst:: Test
testShowSInst = TestCase $ assertEqual "test show a SInst" ("SCall") (show (SCall))
testEqSInst :: Test
testEqSInst= TestCase $ assertEqual "test eq a SInst" (True) ((SCall) == (SCall))

testShowSAsm :: Test
testShowSAsm = TestCase $ assertEqual "test show a SAsm" ("SLabel \"here\"") (show (SLabel "here"))
testEqSAsm :: Test
testEqSAsm = TestCase $ assertEqual "test eq a SASm" (True) ((SLabel "here") == (SLabel "here"))


testListSExprData :: Test
testListSExprData =
    TestList [
        TestLabel "testShowSysCall" testShowSysCall,
        TestLabel "testEqSysCall" testEqSysCall,
        TestLabel "testShowSValue" testShowSValue,
        TestLabel "testEqSValue" testEqSValue,
        TestLabel "testShowSInst" testShowSInst,
        TestLabel "testEqSInst" testEqSInst,
        TestLabel "testShowSAsm" testShowSAsm,
        TestLabel "testEqSAsm" testEqSAsm
    ]
