module UnitTests.UnitTestsSExprToAst(testListSExprToAst) where

import Test.HUnit
import SExprToAst
import SExprData(SExpr(..))
import AstData

assertLeft :: String -> Either a b -> Assertion
assertLeft _ (Left _) = return ()
assertLeft msg (Right _) = assertFailure $ msg ++ " -> expected failure, but got a Right value"

testSuccessBasic :: Test
testSuccessBasic = TestCase $ assertEqual "Test add" (Right (ACall FuncCall {callFunction = FSymbol "+", callArgs = [AInt 1, AInt 2]})) (sExpToAst (SList [SSymbol "+", SInt 1, SInt 2]))
testErrorNoInstruction :: Test
testErrorNoInstruction = TestCase $ assertLeft "Invalid lisp (first value must be an array)" (sExpToAst (SInt 42))

testListSExprToAst :: Test
testListSExprToAst =
  TestList [
    TestLabel "testSuccessBasic" testSuccessBasic,
    TestLabel "testErrorNoInstruction" testErrorNoInstruction
  ]
