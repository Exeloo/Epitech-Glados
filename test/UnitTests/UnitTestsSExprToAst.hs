module UnitTests.UnitTestsSExprToAst(testListSExprToAst) where

import Test.HUnit
import SExprToAst
import SExprData(SExpr(..))
import AstData

assertLeft :: String -> Either a b -> Assertion
assertLeft _ (Left _) = return ()
assertLeft msg (Right _) = assertFailure $ msg ++ " -> expected failure, but got a Right value"

testSuccessBasic :: Test
testSuccessBasic = TestCase $ assertEqual "STOA Test add" (Right (AInt 3)) (sExpToAst (SList [SSymbol "+", SInt 1, SInt 2]))
testErrorNoInstruction :: Test
testErrorNoInstruction = TestCase $ assertLeft "STOA Invalid lisp (first value must be an array)" (sExpToAst (SInt 42))

testSTOALambda :: Test
testSTOALambda = TestCase $ assertEqual "STOA Test Lambda" (Right (AInt 7)) (sExpToAst (SList [SList [SSymbol "lambda", SList [SSymbol "a", SSymbol "b"], SList [SSymbol "+", SSymbol "a", SSymbol "b"]], SInt 3, SInt 4]))
testSTOADefine :: Test
testSTOADefine = TestCase $ assertEqual "STOA Test Define" (Right (AString "#Procedure")) (sExpToAst (SList [SSymbol "define", SSymbol "x", SInt 5]))
testSTOAAdd :: Test
testSTOAAdd = TestCase $ assertEqual "STOA Test Add" (Right (AInt 15)) (sExpToAst (SList [SSymbol "+", SInt 10, SInt 5]))

testListSExprToAst :: Test
testListSExprToAst =
  TestList [
    TestLabel "testSuccessBasic" testSuccessBasic,
    TestLabel "testErrorNoInstruction" testErrorNoInstruction,
    TestLabel "testSTOALambda" testSTOALambda,
    TestLabel "testSTOADefine" testSTOADefine,
    TestLabel "testSTOAAdd" testSTOAAdd
  ]
