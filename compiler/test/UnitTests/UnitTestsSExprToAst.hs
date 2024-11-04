module UnitTests.UnitTestsSExprToAst(testListSExprToAst) where

import Test.HUnit
import SExprToAst
import SExprData
import AstData

testsExpFunctionToAst :: Test
testsExpFunctionToAst = TestCase $ assertEqual "Function to Ast" (Right $ AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = ADeclaration $ FuncDeclaration { declareArgs = ["arg1", "arg2"], declareBody = ASymbol "bar"} }) (sExpFunctionToAst [SSymbol "foo", SParenthesis [SLine [SSymbol "arg1"], SLine [SSymbol "arg2"]], SBracket [SLine [SSymbol "bar"]]])

testsExpFunctionToAstError :: Test
testsExpFunctionToAstError = TestCase $ assertEqual "Function to Ast Error" (Left "Invalid function") (sExpFunctionToAst [SSymbol "foo"])

testsExpFunctionToAstErrorBody :: Test
testsExpFunctionToAstErrorBody = TestCase $ assertEqual "Function to Ast Error Body" (Left "Invalid instruction: [Bracket: {Symbol: \"bar\"}]") (sExpFunctionToAst [SSymbol "foo", SParenthesis [SLine [SSymbol "arg1"], SLine [SSymbol "arg2"]], SBracket [SLine [SBracket [SSymbol "bar"]]]])

testsExpFunctionToAstErrorArgs :: Test
testsExpFunctionToAstErrorArgs = TestCase $ assertEqual "Function to Ast Error Args" (Left "Invalid function symbol: Integer: 1") (sExpFunctionToAst [SSymbol "foo", SParenthesis [SInt 1], SBracket [SLine [SSymbol "bar"]]])

testsExpVarAssignationToAst :: Test
testsExpVarAssignationToAst = TestCase $ assertEqual "VarAssignation to Ast" (Right $ AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = ASymbol "bar" }) (sExpVarAssignationToAst [SSymbol "foo", SSymbol "=", SSymbol "bar"])

testsExpBuilinFunctionToAst :: Test
testsExpBuilinFunctionToAst = TestCase $ assertEqual "BuilinFunction to Ast" (Right $ ACall FuncCall { callFunction = ASymbol "+", callArgs = [AInt 1, AInt 2] }) (sExpBuilinFunctionToAst "+" [SInt 1, SInt 2])

testsExpIfToAst :: Test
testsExpIfToAst = TestCase $ assertEqual "If to Ast" (Right $ ACall FuncCall { callFunction = ASymbol "if", callArgs = [ABool True, AInt 1] }) (sExpIfToAst [SBool True] [SInt 1])

testsExpWhileToAst :: Test
testsExpWhileToAst = TestCase $ assertEqual "While to Ast" (Right $ ALoop $ WhileLoop { whileCondition = ABool True, whileBody = AInt 1 }) (sExpWhileToAst [SBool True] [SInt 1])

testsExpForToAst :: Test
testsExpForToAst = TestCase $ assertEqual "For to Ast" (Right $ ALoop $ ForLoop { forAssignation = [AInt 0], forCondition = ABool True, forIncrementation = [AInt 1], forBody = AInt 1 }) (sExpForToAst [SLine [SInt 0], SLine [SBool True], SLine [SInt 1]] [SLine [SInt 1]])

testsExpInstructionToAst :: Test
testsExpInstructionToAst = TestCase $ assertEqual "Instruction to Ast" (Right $ AInt 1) (sExpInstructionToAst [SInt 1])

testsExpStructToAst :: Test
testsExpStructToAst = TestCase $ assertEqual "Struct to Ast" (Right $ AObject [ObjectElement { objectKey = "foo", objectValue = AInt 1 }]) (sExpStructToAst [[SSymbol "foo",SSymbol ":", SInt 1]])

testsExpVarAssignationToAstError :: Test
testsExpVarAssignationToAstError = TestCase $ assertEqual "VarAssignation to Ast Error" (Left "Invalid assignation: [Symbol: \"foo\"]") (sExpVarAssignationToAst [SSymbol "foo"])

testsExpVarAssignationToAstErrorValue :: Test
testsExpVarAssignationToAstErrorValue = TestCase $ assertEqual "VarAssignation to Ast Error Value" (Left "Invalid instruction: [Bracket: {Line: [Bracket: {Symbol: \"bar\"}]}]") (sExpVarAssignationToAst [SSymbol "foo", SSymbol "=", SBracket [SLine [SBracket[SSymbol "bar"]]]])

testssExpBuilinFunctionToAstError :: Test
testssExpBuilinFunctionToAstError = TestCase $ assertEqual "BuilinFunction to Ast Error" (Left "Invalid builtin function: + with args: [Integer: 1,Integer: 2,Integer: 3]") (sExpBuilinFunctionToAst "+" [SInt 1, SInt 2, SInt 3])

testsExpInstructionToAstInt :: Test
testsExpInstructionToAstInt = TestCase $ assertEqual "Instruction to Ast Int" (Right $ AInt 1) (sExpInstructionToAst [SInt 1])

testsExpInstructionToAstBool :: Test
testsExpInstructionToAstBool = TestCase $ assertEqual "Instruction to Ast Bool" (Right $ ABool True) (sExpInstructionToAst [SBool True])

testsExpInstructionToAstFloat :: Test
testsExpInstructionToAstFloat = TestCase $ assertEqual "Instruction to Ast Float" (Right $ AFloat 1.0) (sExpInstructionToAst [SFloat 1.0])

testsExpInstructionToAstSymbol :: Test
testsExpInstructionToAstSymbol = TestCase $ assertEqual "Instruction to Ast Symbol" (Right $ ASymbol "foo") (sExpInstructionToAst [SSymbol "foo"])

testsExpInstructionToAstArray :: Test
testsExpInstructionToAstArray = TestCase $ assertEqual "Instruction to Ast Array" (Right $ ACall ArrayAccess { accessArray = ASymbol "foo", accessArg = AInt 1 }) (sExpInstructionToAst [SSymbol "foo", SArray [SInt 1]])

testsExpInstructionToAstParenthesis :: Test
testsExpInstructionToAstParenthesis = TestCase $ assertEqual "Instruction to Ast Parenthesis" (Right $ AList [AInt 1]) (sExpInstructionToAst [SParenthesis [SInt 1]])

testsExpInstructionToAstLine :: Test
testsExpInstructionToAstLine = TestCase $ assertEqual "Instruction to Ast Line" (Right $ AInt 1) (sExpInstructionToAst [SLine [SInt 1, SInt 2]])

testsExpInstructionToAstError :: Test
testsExpInstructionToAstError = TestCase $ assertEqual "Instruction to Ast Error" (Left "Invalid instruction: [Bracket: {Line: [Bracket: {Symbol: \"bar\"}]}]") (sExpInstructionToAst [SBracket [SLine [SBracket[SSymbol "bar"]]]])

testsExpInstructionToAstfunction :: Test
testsExpInstructionToAstfunction = TestCase $ assertEqual "Instruction to Ast Function" (Right $ AAssignation (VarAssignation {assignationKey = "foo", assignationValue = ADeclaration (FuncDeclaration {declareArgs = ["arg1","arg2"], declareBody = ASymbol "bar"})})) (sExpInstructionToAst [SSymbol "function",SSymbol "foo", SParenthesis [SLine [SSymbol "arg1"], SLine [SSymbol "arg2"]], SBracket [SLine [SSymbol "bar"]]])

testsExpInstructionToAstVarAssignationLet :: Test
testsExpInstructionToAstVarAssignationLet = TestCase $ assertEqual "Instruction to Ast VarAssignation" (Right $ AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = ASymbol "bar" }) (sExpInstructionToAst [SSymbol "let", SSymbol "foo", SSymbol "=", SSymbol "bar"])

testsExpInstructionToAstVarAssignation :: Test
testsExpInstructionToAstVarAssignation = TestCase $ assertEqual "Instruction to Ast VarAssignation" (Right $ AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = ASymbol "bar" }) (sExpInstructionToAst [SSymbol "foo", SSymbol "=", SSymbol "bar"])

testsExpInstructionToAstBreak :: Test
testsExpInstructionToAstBreak = TestCase $ assertEqual "Instruction to Ast Break" (Right $ ACall FuncCall { callFunction = ASymbol "break", callArgs = [] }) (sExpInstructionToAst [SSymbol "break"])

testsExpInstructionToAstContinue :: Test
testsExpInstructionToAstContinue = TestCase $ assertEqual "Instruction to Ast Continue" (Right $ ACall FuncCall { callFunction = ASymbol "continue", callArgs = [] }) (sExpInstructionToAst [SSymbol "continue"])

testsExpInstructionToAstReturn :: Test
testsExpInstructionToAstReturn = TestCase $ assertEqual "Instruction to Ast Return" (Right $ ACall FuncCall { callFunction = ASymbol "return", callArgs = [AInt 1] }) (sExpInstructionToAst [SSymbol "return", SInt 1])

testsExpInstructionToAstPrint :: Test
testsExpInstructionToAstPrint = TestCase $ assertEqual "Instruction to Ast Print" (Right $ ACall FuncCall { callFunction = ASymbol "print", callArgs = [AInt 1] }) (sExpInstructionToAst [SSymbol "print", SInt 1])

testsExpInstructionToAstBuilinFunction :: Test
testsExpInstructionToAstBuilinFunction = TestCase $ assertEqual "Instruction to Ast BuilinFunction" (Right $ ACall FuncCall { callFunction = ASymbol "+", callArgs = [AInt 1, AInt 2] }) (sExpInstructionToAst [SInt 1, SSymbol "+", SInt 2])

testsExpInstructionToAstIf :: Test
testsExpInstructionToAstIf = TestCase $ assertEqual "Instruction to Ast If" (Right $ ACall FuncCall { callFunction = ASymbol "if", callArgs = [ABool True, AInt 1] }) (sExpInstructionToAst [SSymbol "if", SBool True, SInt 1])

testsExpInstructionToAstWhile :: Test
testsExpInstructionToAstWhile = TestCase $ assertEqual "Instruction to Ast While" (Right $ ALoop $ WhileLoop { whileCondition = ABool True, whileBody = AInt 1 }) (sExpInstructionToAst [SSymbol "while", SParenthesis [SBool True], SBracket [SLine [SInt 1]]])

testsExpInstructionToAstFor :: Test
testsExpInstructionToAstFor = TestCase $ assertEqual "Instruction to Ast For" (Right $ ALoop $ ForLoop { forAssignation = [AInt 0], forCondition = ABool True, forIncrementation = [AInt 1], forBody = AInt 1 }) (sExpInstructionToAst [SSymbol "for", SParenthesis [SLine [SInt 0], SLine [SBool True], SLine [SInt 1]], SBracket [SLine [SInt 1]]])

testsExpInstructionToAstArrayAccess :: Test
testsExpInstructionToAstArrayAccess = TestCase $ assertEqual "Instruction to Ast ArrayAccess" (Right $ ACall ArrayAccess { accessArray = ASymbol "foo", accessArg = AInt 1 }) (sExpInstructionToAst [SSymbol "foo", SArray [SInt 1]])

testsExpInstructionToAstStruct :: Test
testsExpInstructionToAstStruct = TestCase $ assertEqual "Instruction to Ast Struct" (Right $ AObject [ObjectElement { objectKey = "foo", objectValue = AInt 1 }]) (sExpInstructionToAst [SStruct [[SSymbol "foo",SSymbol ":", SInt 1]]])

testListSExprToAst :: Test
testListSExprToAst =
  TestList [
    testsExpFunctionToAst,
    testsExpVarAssignationToAst,
    testsExpBuilinFunctionToAst,
    testsExpIfToAst,
    testsExpWhileToAst,
    testsExpForToAst,
    testsExpInstructionToAst,
    testsExpStructToAst,
    testsExpFunctionToAstError,
    testsExpFunctionToAstErrorBody,
    testsExpFunctionToAstErrorArgs,
    testsExpVarAssignationToAstError,
    testsExpVarAssignationToAstErrorValue,
    testssExpBuilinFunctionToAstError,
    testsExpInstructionToAstInt,
    testsExpInstructionToAstBool,
    testsExpInstructionToAstFloat,
    testsExpInstructionToAstSymbol,
    testsExpInstructionToAstArray,
    testsExpInstructionToAstParenthesis,
    testsExpInstructionToAstLine,
    testsExpInstructionToAstError,
    testsExpInstructionToAstfunction,
    testsExpInstructionToAstVarAssignationLet,
    testsExpInstructionToAstVarAssignation,
    testsExpInstructionToAstBreak,
    testsExpInstructionToAstContinue,
    testsExpInstructionToAstReturn,
    testsExpInstructionToAstPrint,
    testsExpInstructionToAstBuilinFunction,
    testsExpInstructionToAstIf,
    testsExpInstructionToAstWhile,
    testsExpInstructionToAstFor,
    testsExpInstructionToAstArrayAccess,
    testsExpInstructionToAstStruct
  ]
