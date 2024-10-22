module UnitTests.UnitTestsSExprData(testListSExprData) where

import Test.HUnit
import SExprData(SExpr(..))

testShowSInt :: Test
testShowSInt = TestCase $ assertEqual "Show SInt" "Integer: 42" (show (SInt 42))

testShowSFloat :: Test
testShowSFloat = TestCase $ assertEqual "Show SFloat" "Float: 42.42" (show (SFloat 42.42))

testShowSBool :: Test
testShowSBool = TestCase $ assertEqual "Show SBool" "Boolean: True" (show (SBool True))

testShowSSymbol :: Test
testShowSSymbol = TestCase $ assertEqual "Show SSymbol" "Symbol: \"define\"" (show (SSymbol "define"))

testShowSArray :: Test
testShowSArray = TestCase $ assertEqual "Show SArray" "Array: [Integer: 42, Integer: 84, Integer: 21]" (show (SArray [SInt 42, SInt 84, SInt 21]))

testShowSList :: Test
testShowSList = TestCase $ assertEqual "Show SList" "List: [Integer: 42, Symbol: \"define\"]" (show (SList [SInt 42, SSymbol "define"]))

testShowSStruct :: Test
testShowSStruct = TestCase $ assertEqual "Show SStruct" "Struct: {\"question\": String: \"Où est la pierre ?\", \"answer\": String: \"Dans la poche !\", \"flag-triche\": Integer: 84}"
    (show (SStruct [("question", SString "Où est la pierre ?"), ("answer", SString "Dans la poche !"), ("flag-triche", SInt 84)]))

testShowSString :: Test
testShowSString = TestCase $ assertEqual "Show SString" "String: \"hello\"" (show (SString "hello"))

testShowUndefined :: Test
testShowUndefined = TestCase $ assertEqual "Show Undefined" "Undefined" (show Undefined)

testEqSInt :: Test
testEqSInt = TestCase $ assertEqual "Eq SInt" True ((SInt 42) == (SInt 42))

testEqSFloat :: Test
testEqSFloat = TestCase $ assertEqual "Eq SFloat" True ((SFloat 42.42) == (SFloat 42.42))

testEqSBool :: Test
testEqSBool = TestCase $ assertEqual "Eq SBool" True ((SBool True) == (SBool True))

testEqSSymbol :: Test
testEqSSymbol = TestCase $ assertEqual "Eq SSymbol" False ((SSymbol "define") == (SSymbol "lambda"))

testEqSList :: Test
testEqSList = TestCase $ assertEqual "Eq SList" True ((SList [SInt 42, SSymbol "lambda"]) == (SList [SInt 42, SSymbol "lambda"]))

testEqSArray :: Test
testEqSArray = TestCase $ assertEqual "Eq SArray" True ((SArray [SInt 42, SInt 84]) == (SArray [SInt 42, SInt 84]))

testEqSString :: Test
testEqSString = TestCase $ assertEqual "Eq SString" False ((SString "hello") == (SString "goodbye"))

testEqSStruct :: Test
testEqSStruct = TestCase $ assertEqual "Eq SStruct" True ((SStruct [("pierre", SString "dans la poche")]) == (SStruct [("pierre", SString "dans la poche")]))

testEqUndefined :: Test
testEqUndefined = TestCase $ assertEqual "Eq Undefined" True (Undefined == Undefined)

testEqDifferentTypes :: Test
testEqDifferentTypes = TestCase $ assertEqual "Eq Different Types" False ((SInt 42) == (SSymbol "define"))

testListSExprData :: Test
testListSExprData =
  TestList [
    TestLabel "testShowSInt" testShowSInt,
    TestLabel "testShowSFloat" testShowSFloat,
    TestLabel "testShowSBool" testShowSBool,
    TestLabel "testShowSSymbol" testShowSSymbol,
    TestLabel "testShowSArray" testShowSArray,
    TestLabel "testShowSList" testShowSList,
    TestLabel "testShowSStruct" testShowSStruct,
    TestLabel "testShowSString" testShowSString,
    TestLabel "testShowUndefined" testShowUndefined,
    TestLabel "testEqSInt" testEqSInt,
    TestLabel "testEqSFloat" testEqSFloat,
    TestLabel "testEqSBool" testEqSBool,
    TestLabel "testEqSSymbol" testEqSSymbol,
    TestLabel "testEqSList" testEqSList,
    TestLabel "testEqSArray" testEqSArray,
    TestLabel "testEqSString" testEqSString,
    TestLabel "testEqSStruct" testEqSStruct,
    TestLabel "testEqUndefined" testEqUndefined,
    TestLabel "testEqDifferentTypes" testEqDifferentTypes
  ]