module UnitTests.UnitTestsSExprToAst(testListSExprToAst) where

import Test.HUnit
import SExprToAst
import SExprData
import AstData

TestsExpFunctionToAst :: Test
TestsExpFunctionToAst = TestCase $ assertEqual "Function to Ast" (Right $ AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = ADeclaration $ FuncDeclaration { declareArgs = ["arg1", "arg2"], declareBody = [ASymbol "bar"] } }) (sExpFunctionToAst [SSymbol "foo", SParenthesis [SSymbol "arg1", SSymbol "arg2"], SBracket [SLine [SSymbol "bar"]]])

TestsExpVarAssignationToAst :: Test
TestsExpVarAssignationToAst = TestCase $ assertEqual "VarAssignation to Ast" (Right $ AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = ASymbol "bar" }) (sExpVarAssignationToAst [SSymbol "foo", SSymbol "=", SSymbol "bar"])

TestsExpBuilinFunctionToAst :: Test
TestsExpBuilinFunctionToAst = TestCase $ assertEqual "BuilinFunction to Ast" (Right $ ACall FuncCall { callFunction = ASymbol "+", callArgs = [AInt 1, AInt 2] }) (sExpBuilinFunctionToAst "+" [SInt 1, SInt 2])

TestsExpIfToAst :: Test
TestsExpIfToAst = TestCase $ assertEqual "If to Ast" (Right $ ACall FuncCall { callFunction = ASymbol "if", callArgs = [ABool True, AInt 1] }) (sExpIfToAst [SBool True, SInt 1])

TestsExpWhileToAst :: Test
TestsExpWhileToAst = TestCase $ assertEqual "While to Ast" (Right $ ALoop $ WhileLoop { whileCondition = ABool True, whileBody = [AInt 1] }) (sExpWhileToAst [SBool True, SInt 1])

TestsExpForToAst :: Test
TestsExpForToAst = TestCase $ assertEqual "For to Ast" (Right $ ALoop $ ForLoop { forAssignation = [AInt 0], forCondition = [ABool True], forIncrementation = [AInt 1], forBody = [AInt 1] }) (sExpForToAst [SInt 0, SBool True, SInt 1, SInt 1])

TestsExpInstructionToAst :: Test
TestsExpInstructionToAst = TestCase $ assertEqual "Instruction to Ast" (Right $ AInt 1) (sExpInstructionToAst [SInt 1])

TestsExpStructToAst :: Test
TestsExpStructToAst = TestCase $ assertEqual "Struct to Ast" (Right $ AObject [ObjectElement { objectKey = "foo", objectValue = AInt 1 }]) (sExpStructToAst [SStruct [[SSymbol "foo", SInt 1]]])

testListSExprToAst :: Test
testListSExprToAst =
  TestList [
    TestsExpFunctionToAst,
    TestsExpVarAssignationToAst,
    TestsExpBuilinFunctionToAst,
    TestsExpIfToAst,
    TestsExpWhileToAst,
    TestsExpForToAst,
    TestsExpInstructionToAst,
    TestsExpStructToAst
  ]
