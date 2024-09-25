import Test.HUnit
import AstEval
import AstData

-- Test cases for callAST
testCallASTAdd = TestCase $ assertEqual "10 + 5 = 15" (Just (AInt 15)) (callAST "+" (AList [AInt 10, AInt 5]))
testCallASTSub = TestCase $ assertEqual "10 - 5 = 5" (Just (AInt 5)) (callAST "-" (AList [AInt 10, AInt 5]))
testCallASTMul = TestCase $ assertEqual "10 * 5 = 50" (Just (AInt 50)) (callAST "*" (AList [AInt 10, AInt 5]))
testCallASTDiv = TestCase $ assertEqual "10 `div` 5 = 2" (Just (AInt 2)) (callAST "div" (AList [AInt 10, AInt 5]))
testCallASTMod = TestCase $ assertEqual "10 `mod` 3 = 1" (Just (AInt 1)) (callAST "mod" (AList [AInt 10, AInt 3]))
testCallASTEq = TestCase $ assertEqual "10 == 10" (Just (ABool True)) (callAST "eq?" (AList [AInt 10, AInt 10]))
testCallASTLt = TestCase $ assertEqual "10 < 5" (Just (ABool False)) (callAST "<" (AList [AInt 10, AInt 5]))

testReplaceSymbol = TestCase $ assertEqual "Replace symbol" (AInt 5) (replaceSymbol (ASymbol "x") (ASymbol "x") (AInt 5) )

testEvalASTInt = TestCase $ assertEqual "Eval AInt" (Just (AInt 10)) (evalAST (AInt 10))
testEvalASTSymbol = TestCase $ assertEqual "Eval ASymbol" (Just (ASymbol "x")) (evalAST (ASymbol "x"))
testEvalASTAdd = TestCase $ assertEqual "Eval Add" (Just (AInt 15)) (evalAST (ACall FuncCall {callFunction = FSymbol "+", callArgs = [AInt 10, AInt 5]}))

testlist :: Test
testlist = TestList [
  TestLabel "testCallASTAdd" testCallASTAdd,
  TestLabel "testCallASTSub" testCallASTSub,
  TestLabel "testCallASTMul" testCallASTMul,
  TestLabel "testCallASTDiv" testCallASTDiv,
  TestLabel "testCallASTMod" testCallASTMod,
  TestLabel "testCallASTEq" testCallASTEq,
  TestLabel "testCallASTLt" testCallASTLt,
  TestLabel "testReplaceSymbol" testReplaceSymbol,
  TestLabel "testEvalASTInt" testEvalASTInt,
  TestLabel "testEvalASTSymbol" testEvalASTSymbol,
  TestLabel "testEvalASTAdd" testEvalASTAdd
  ]

main :: IO ()
main = do
    runTestTT testlist
    return ()
