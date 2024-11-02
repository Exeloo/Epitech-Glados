module UnitTests.UnitTestsExecInstruction(testListExecInstruction) where

import Test.HUnit
import InstructionData
import ExecInstructions


testExec1 :: Test
testExec1 = TestCase $ assertEqual "check result for Ret instruction" (Right "test") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(Ret)]) ([(VString "here")]))
testExec2 :: Test
testExec2 = TestCase $ assertEqual "check result for Call Add instruction" (Left "Add need two numbers") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call)]) ([(VCall Add)]))
testExec3 :: Test
testExec3 = TestCase $ assertEqual "check result for Call Sub instruction" (Left "Sub need two numbers") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call)]) ([(VCall Sub)]))
testExec4 :: Test
testExec4 = TestCase $ assertEqual "check result for Call Mul instruction" (Left "Mul need two numbers") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call)]) ([(VCall Mul)]))
testExec5 :: Test
testExec5 = TestCase $ assertEqual "check result for Call Div instruction" (Left "Div need two numbers") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call)]) ([(VCall Div)]))
testExec6 :: Test
testExec6 = TestCase $ assertEqual "check result for Call Eq instruction" (Left "Eq need two value to compare") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call)]) ([(VCall Eq)]))
testExec7 :: Test
testExec7 = TestCase $ assertEqual "check result for Call Less instruction" (Left "Less need two numbers") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call)]) ([(VCall Less)]))
testExec8 :: Test
testExec8 = TestCase $ assertEqual "check result for Call Not instruction" (Left "Not need a value to be call") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call)]) ([(VCall Not)]))
testExec9 :: Test
testExec9 = TestCase $ assertEqual "check result for Call Or instruction" (Left "Or need two bool to compare") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call)]) ([(VCall Or)]))
testExec10 :: Test
testExec10 = TestCase $ assertEqual "check result for Call And instruction" (Left "And need two bool to compare") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call)]) ([(VCall And)]))
testExec11 :: Test
testExec11 = TestCase $ assertEqual "check result for Call Print instruction" (Left "Print need a value to be call") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call)]) ([(VCall Print)]))


testListExecInstruction :: Test
testListExecInstruction =
    TestList [
        TestLabel "testExec1" testExec1,
        TestLabel "testExec2" testExec2,
        TestLabel "testExec3" testExec3,
        TestLabel "testExec4" testExec4,
        TestLabel "testExec5" testExec5,
        TestLabel "testExec6" testExec6,
        TestLabel "testExec7" testExec7,
        TestLabel "testExec8" testExec8,
        TestLabel "testExec9" testExec9,
        TestLabel "testExec10" testExec10,
        TestLabel "testExec11" testExec11
    ]
