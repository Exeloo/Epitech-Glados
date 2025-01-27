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

testShowSLine :: Test
testShowSLine = TestCase $ assertEqual "Show SLine" "Line: [Integer: 42, Symbol: \"define\"]" (show (SLine [SInt 42, SSymbol "define"]))

testShowSParenthesis :: Test
testShowSParenthesis = TestCase $ assertEqual "Show SParenthesis" "Parenthesis: (Symbol: \"foo\", Symbol: \">\", Integer: 42)" (show (SParenthesis [SSymbol "foo", SSymbol ">", SInt 42]))

testShowSBracket :: Test
testShowSBracket = TestCase $ assertEqual "Show SBracket" "Bracket: {Line: [Symbol: \"foo\", Symbol: \"=\", Integer: 84], Line: [Symbol: \"foo\", Symbol: \">\", Integer: 42]}" (show (SBracket [(SLine [SSymbol "foo", SSymbol "=", SInt 84]), (SLine [SSymbol "foo", SSymbol ">", SInt 42])]))

testShowSStruct :: Test
testShowSStruct = TestCase $ assertEqual "Show SStruct" "Struct: {[Symbol: \"question\", String: \"Où est la pierre ?\"], [Symbol: \"answer\", String: \"Dans la poche !\"]}"
    (show (SStruct [[SSymbol "question", SString "Où est la pierre ?"], [SSymbol "answer", SString "Dans la poche !"]]))

testShowSString :: Test
testShowSString = TestCase $ assertEqual "Show SString" "String: \"hello\"" (show (SString "hello"))

testEqSInt :: Test
testEqSInt = TestCase $ assertEqual "Eq SInt" True ((SInt 42) == (SInt 42))

testEqSFloat :: Test
testEqSFloat = TestCase $ assertEqual "Eq SFloat" True ((SFloat 42.42) == (SFloat 42.42))

testEqSBool :: Test
testEqSBool = TestCase $ assertEqual "Eq SBool" True ((SBool True) == (SBool True))

testEqSSymbol :: Test
testEqSSymbol = TestCase $ assertEqual "Eq SSymbol" False ((SSymbol "define") == (SSymbol "lambda"))

testEqSLine :: Test
testEqSLine = TestCase $ assertEqual "Eq SLine" True ((SLine [SInt 42, SSymbol "lambda"]) == (SLine [SInt 42, SSymbol "lambda"]))

testEqSArray :: Test
testEqSArray = TestCase $ assertEqual "Eq SArray" True ((SArray [SInt 42, SInt 84]) == (SArray [SInt 42, SInt 84]))

testEqSString :: Test
testEqSString = TestCase $ assertEqual "Eq SString" False ((SString "hello") == (SString "goodbye"))

testEqSStruct :: Test
testEqSStruct = TestCase $ assertEqual "Eq SStruct" True
    (SStruct [[SSymbol "Quoi", SString "la pierre"], [SSymbol "Où", SString "Dans la poche"]] ==
     SStruct [[SSymbol "Quoi", SString "la pierre"], [SSymbol "Où", SString "Dans la poche"]])

testEqSParenthesis :: Test
testEqSParenthesis = TestCase $ assertEqual "Eq SParenthesis" True
    (SParenthesis [SSymbol "foo", SSymbol "=", SInt 84] ==
    SParenthesis [SSymbol "foo", SSymbol "=", SInt 84])

testEqSBracket :: Test
testEqSBracket = TestCase $ assertEqual "Eq SBracket" True
    (SBracket [SLine [SSymbol "foo", SSymbol ">", SInt 42]] ==
    SBracket [SLine [SSymbol "foo", SSymbol ">", SInt 42]])

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
    TestLabel "testShowSLine" testShowSLine,
    TestLabel "testShowSStruct" testShowSStruct,
    TestLabel "testShowSParenthesis" testShowSParenthesis,
    TestLabel "testShowSBracket" testShowSBracket,
    TestLabel "testShowSString" testShowSString,
    TestLabel "testEqSInt" testEqSInt,
    TestLabel "testEqSFloat" testEqSFloat,
    TestLabel "testEqSBool" testEqSBool,
    TestLabel "testEqSSymbol" testEqSSymbol,
    TestLabel "testEqSLine" testEqSLine,
    TestLabel "testEqSArray" testEqSArray,
    TestLabel "testEqSString" testEqSString,
    TestLabel "testEqSStruct" testEqSStruct,
    TestLabel "testEqSParenthesis" testEqSParenthesis,
    TestLabel "testEqSBracket" testEqSBracket,
    TestLabel "testEqDifferentTypes" testEqDifferentTypes
  ]