module UnitTests.UnitTestsSExprToAst(testListSExprToAst) where

import Test.HUnit
import SExprToAst
import SExprData(SExpr(..))
import AstData

assertLeft :: String -> Either a b -> Assertion
assertLeft _ (Left _) = return ()
assertLeft msg (Right _) = assertFailure $ msg ++ " -> expected failure, but got a Right value"

testSuccessBasic :: Test
testSuccessBasic = TestCase $ assertEqual "STOA Test add" (Right (ACall FuncCall {callFunction = FSymbol "+", callArgs = [AInt 1, AInt 2]})) (sExpToAst (SList [SSymbol "+", SInt 1, SInt 2]))
testErrorNoInstruction :: Test
testErrorNoInstruction = TestCase $ assertLeft "STOA Invalid lisp (first value must be an array)" (sExpToAst (SInt 42))

testSTOALambda :: Test
testSTOALambda = TestCase $ assertEqual "STOA Test Lambda" (Right (ACall FuncCall {callFunction = FFunc FuncDeclaration {declareArgs = ["a", "b"], declareBody = [ACall FuncCall {callFunction = FSymbol "+", callArgs = [ASymbol "a", ASymbol "b"]}]}, callArgs = [AInt 3, AInt 4]})) (sExpToAst (SList [SList [SSymbol "lambda", SList [SSymbol "a", SSymbol "b"], SList [SSymbol "+", SSymbol "a", SSymbol "b"]], SInt 3, SInt 4]))
testSTOADefine :: Test
testSTOADefine = TestCase $ assertEqual "STOA Test Define" (Right (AAssignation (VarAssignation "x" (AInt 5)))) (sExpToAst (SList [SSymbol "define", SSymbol "x", SInt 5]))
testSTOAAdd :: Test
testSTOAAdd = TestCase $ assertEqual "STOA Test Add" (Right (ACall FuncCall {callFunction = FSymbol "+", callArgs = [AInt 10, AInt 5]})) (sExpToAst (SList [SSymbol "+", SInt 10, SInt 5]))

testListSExprToAst :: Test
testListSExprToAst =
  TestList [
    TestLabel "testSuccessBasic" testSuccessBasic,
    TestLabel "testErrorNoInstruction" testErrorNoInstruction,
    TestLabel "testSTOALambda" testSTOALambda,
    TestLabel "testSTOADefine" testSTOADefine,
    TestLabel "testSTOAAdd" testSTOAAdd
  ]
