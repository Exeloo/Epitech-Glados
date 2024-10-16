module UnitTests.UnitTestsSExprData(testListSExprData) where

import Test.HUnit
import SExprData(SExpr(..))

testShowSInt :: Test
testShowSInt = TestCase $ assertEqual "Show SInt" "Integer: 42" (show (SInt 42))

testShowSSymbol :: Test
testShowSSymbol = TestCase $ assertEqual "Show SSymbol" "Symbol: \"define\"" (show (SSymbol "define"))

testShowSList :: Test
testShowSList = TestCase $ assertEqual "Show SList" "List: [Integer: 42, Symbol: \"define\"]" (show (SList [SInt 42, SSymbol "define"]))

testShowSString :: Test
testShowSString = TestCase $ assertEqual "Show SString" "String: \"hello\"" (show (SString "hello"))

testEqSInt :: Test
testEqSInt = TestCase $ assertEqual "Eq SInt" True ((SInt 42) == (SInt 42))

testEqSSymbol :: Test
testEqSSymbol = TestCase $ assertEqual "Eq SSymbol" False ((SSymbol "define") == (SSymbol "lambda"))

testEqSList :: Test
testEqSList = TestCase $ assertEqual "Eq SList" True ((SList [SInt 42, SSymbol "lambda"]) == (SList [SInt 42, SSymbol "lambda"]))

testEqSString :: Test
testEqSString = TestCase $ assertEqual "Eq SString" False ((SString "hello") == (SString "goodbye"))

testEqDifferentTypes :: Test
testEqDifferentTypes = TestCase $ assertEqual "Eq Different Types" False ((SInt 42) == (SSymbol "define"))

testListSExprData :: Test
testListSExprData =
  TestList [
    TestLabel "testShowSInt" testShowSInt,
    TestLabel "testShowSSymbol" testShowSSymbol,
    TestLabel "testShowSList" testShowSList,
    TestLabel "testShowSString" testShowSString,
    TestLabel "testEqSInt" testEqSInt,
    TestLabel "testEqSSymbol" testEqSSymbol,
    TestLabel "testEqSList" testEqSList,
    TestLabel "testEqSString" testEqSString,
    TestLabel "testEqDifferentTypes" testEqDifferentTypes
  ]