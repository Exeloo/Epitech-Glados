module UnitTests.UnitTestAstEval (testlistAstEval) where

import Test.HUnit
import AstEval
import AstData

testCallASTAdd :: Test
testCallASTAdd = TestCase $ assertEqual "10 + 5 = 15" (Right (AInt 15)) (callAST "+" [AInt 10, AInt 5])
testCallASTSub :: Test
testCallASTSub = TestCase $ assertEqual "10 - 5 = 5" (Right (AInt 5)) (callAST "-" [AInt 10, AInt 5])
testCallASTMul :: Test
testCallASTMul = TestCase $ assertEqual "10 * 5 = 50" (Right (AInt 50)) (callAST "*" [AInt 10, AInt 5])
testCallASTDiv :: Test
testCallASTDiv = TestCase $ assertEqual "10 `div` 5 = 2" (Right (AInt 2)) (callAST "div" [AInt 10, AInt 5])
testCallASTMod :: Test
testCallASTMod = TestCase $ assertEqual "10 `mod` 3 = 1" (Right (AInt 1)) (callAST "mod" [AInt 10, AInt 3])
testCallASTEq :: Test
testCallASTEq = TestCase $ assertEqual "10 == 10" (Right (ABool True)) (callAST "eq?" [AInt 10, AInt 10])
testCallASTLt :: Test
testCallASTLt = TestCase $ assertEqual "10 < 5" (Right (ABool False)) (callAST "<" [AInt 10, AInt 5])
testCallASTIf :: Test
testCallASTIf = TestCase $ assertEqual "if true then 5 else 10" (Right (AInt 5)) (callAST "if" [ABool True, AInt 5, AInt 10])

testReplaceSymbol :: Test
testReplaceSymbol = TestCase $ assertEqual "Replace symbol" (AInt 5) (replaceSymbol "x" (AInt 5) [[]] (ASymbol "x"))

testEvalASTDefine :: Test
testEvalASTDefine = TestCase $ assertEqual "Eval AST Define" (Right (AInt 4)) (evalAST [[]] (ACall FuncCall {callFunction = FFunc FuncDeclaration {declareArgs = ["a", "b"], declareBody = [ACall FuncCall {callFunction = FSymbol "+", callArgs = [ASymbol "a", ASymbol "b"]}]}, callArgs = [AInt 3, AInt 4]}))
testEvalASTInt :: Test
testEvalASTInt = TestCase $ assertEqual "Eval AST Int" (Right (AInt 5)) (evalAST [[]] (AInt 5))
testEvalASTSymbol :: Test
testEvalASTSymbol = TestCase $ assertEqual "Eval AST Symbol" (Right (ASymbol x)) (evalAST [[AAssignation (VarAssignation "x" (AInt 5))]] (ASymbol "x"))
testEvalASTAdd :: Test
testEvalASTAdd = TestCase $ assertEqual "Eval AST Add" (Right (AInt 15)) (evalAST [[]] (ACall FuncCall {callFunction = FSymbol "+", callArgs = [AInt 10, AInt 5]}))

testlistAstEval :: Test
testlistAstEval = TestList [
  TestLabel "testCallASTAdd" testCallASTAdd,
  TestLabel "testCallASTSub" testCallASTSub,
  TestLabel "testCallASTMul" testCallASTMul,
  TestLabel "testCallASTDiv" testCallASTDiv,
  TestLabel "testCallASTMod" testCallASTMod,
  TestLabel "testCallASTEq" testCallASTEq,
  TestLabel "testCallASTLt" testCallASTLt,
  TestLabel "testCallASTIf" testCallASTIf,
  TestLabel "testReplaceSymbol" testReplaceSymbol,
  TestLabel "testEvalASTDefine" testEvalASTDefine,
  TestLabel "testEvalASTInt" testEvalASTInt,
  TestLabel "testEvalASTSymbol" testEvalASTSymbol,
  TestLabel "testEvalASTAdd" testEvalASTAdd
  ]
