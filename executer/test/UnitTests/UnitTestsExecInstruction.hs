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

testExec12 :: Test
testExec12 = TestCase $ assertEqual "check result to PutOnStack element with invalid length" (Left "Can't push empty stack on arg") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(PushStackOnArg), (PushStackOnArg)]) ([(VInt 42)]))
testExec13 :: Test
testExec13 = TestCase $ assertEqual "check result to PopArg element with empty list" (Left "Can't pop empty arg") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(PopArg), (PopArg)]) ([(VInt 42)]))
testExec14 :: Test
testExec14 = TestCase $ assertEqual "check result to PopStack element with empty list" (Left "Can't pop empty stack") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(PopStack), (PopStack)]) ([(VInt 42)]))
testExec15 :: Test
testExec15 = TestCase $ assertEqual "check result with no Ret at the end" (Left "No Ret at end of instructions") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([]) ([(VInt 42)]))

testExec16 :: Test
testExec16 = TestCase $ assertEqual "check result pushing on stack" (Right "84") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Push (VInt 84)), (Push (VCall (Print))), (Call), (Ret)]) ([(VInt 42)]))
testExec17 :: Test
testExec17 = TestCase $ assertEqual "check result for add int call" (Right "3") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Add), (VInt 1), (VInt 2)]))
testExec18 :: Test
testExec18 = TestCase $ assertEqual "check result for add double call" (Right "3.0") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Add), (VDouble 1), (VDouble 2)]))
testExec19 :: Test
testExec19 = TestCase $ assertEqual "check result for sub int call" (Right "1") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Sub), (VInt 2), (VInt 1)]))
testExec20 :: Test
testExec20 = TestCase $ assertEqual "check result for sub double call" (Right "1.0") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Sub), (VDouble 2), (VDouble 1)]))
testExec21 :: Test
testExec21 = TestCase $ assertEqual "check result for mul int call" (Right "2") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Mul), (VInt 1), (VInt 2)]))
testExec22 :: Test
testExec22 = TestCase $ assertEqual "check result for mul double call" (Right "2.0") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Mul), (VDouble 1), (VDouble 2)]))
testExec23 :: Test
testExec23 = TestCase $ assertEqual "check result for div int call" (Right "2") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Div), (VInt 2), (VInt 1)]))
testExec24 :: Test
testExec24 = TestCase $ assertEqual "check result for div double call" (Right "2.0") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Div), (VDouble 2), (VDouble 1)]))
testExec25 :: Test
testExec25 = TestCase $ assertEqual "check result for div 0 int call" (Left "Division by 0") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Ret)]) ([(VCall Div), (VInt 2), (VInt 0)]))
testExec26 :: Test
testExec26 = TestCase $ assertEqual "check result for div 0 double call" (Left "Division by 0") (exec "test" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Ret)]) ([(VCall Div), (VDouble 2), (VDouble 0)]))

testExec27 :: Test
testExec27 = TestCase $ assertEqual "check result for eq call" (Right "False") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Eq), (VDouble 2), (VDouble 0)]))
testExec28 :: Test
testExec28 = TestCase $ assertEqual "check result for less int call" (Right "False") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Less), (VInt 2), (VInt 0)]))
testExec29 :: Test
testExec29 = TestCase $ assertEqual "check result for less double call" (Right "False") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Less), (VDouble 2), (VDouble 0)]))

testExec30 :: Test
testExec30 = TestCase $ assertEqual "check result for not int call" (Right "False") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Not), (VInt 2)]))
testExec31 :: Test
testExec31 = TestCase $ assertEqual "check result for not double call" (Right "False") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Not), (VDouble 2.0)]))
testExec32 :: Test
testExec32 = TestCase $ assertEqual "check result for not bool call" (Right "False") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Not), (VBool True)]))
testExec33 :: Test
testExec33 = TestCase $ assertEqual "check result for not string call" (Right "False") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Not), (VString "here")]))
testExec34 :: Test
testExec34 = TestCase $ assertEqual "check result for not array call" (Right "True") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Not), (VArray [])]))
testExec35 :: Test
testExec35 = TestCase $ assertEqual "check result for not object call" (Right "False") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Not), (VObject [("here", (VInt 5))])]))
testExec36 :: Test
testExec36 = TestCase $ assertEqual "check result for not undefined call" (Right "True") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Not), (VUndefined)]))

testTakeListEnd1 :: Test
testTakeListEnd1 = TestCase $ assertEqual "check result normal list" ([3,4]) (takeListEnd [1,2,3,4] 2)
testTakeListEnd2 :: Test
testTakeListEnd2 = TestCase $ assertEqual "check result empty list" ([]) (takeListEnd [1] 10)

testExec37 :: Test
testExec37 = TestCase $ assertEqual "check result for print string call" (Right "here") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Ret)]) ([(VCall Print), (VString "here")]))
testExec38 :: Test
testExec38 = TestCase $ assertEqual "check result for print array call" (Right "[(0,VString \"here\")]") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Ret)]) ([(VCall Print), (VArray [(0, (VString "here"))])]))
testExec39 :: Test
testExec39 = TestCase $ assertEqual "check result for print object call" (Right "[(\"A\",VString \"here\")]") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Ret)]) ([(VCall Print), (VObject [("A", (VString "here"))])]))
testExec40 :: Test
testExec40 = TestCase $ assertEqual "check result for print object call" (Right "Undefined") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Ret)]) ([(VCall Print), (VUndefined)]))

testExec41 :: Test
testExec41 = TestCase $ assertEqual "check result for call on empty stack" (Left "Can't Call on empty stack") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call)]) ([]))
testExec42 :: Test
testExec42 = TestCase $ assertEqual "check result for or undefined call" (Right "True") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall Or), (VBool True), (VBool False)]))
testExec43 :: Test
testExec43 = TestCase $ assertEqual "check result for and undefined call" (Right "False") (exec "" ([(Push (VInt 5))]) ([(VBool True)]) ([(Call), (Push (VCall (Print))), (Call), (Ret)]) ([(VCall And), (VBool True), (VBool False)]))


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
        TestLabel "testExec11" testExec11,
        TestLabel "testExec12" testExec12,
        TestLabel "testExec13" testExec13,
        TestLabel "testExec14" testExec14,
        TestLabel "testExec15" testExec15,
        TestLabel "testExec16" testExec16,
        TestLabel "testExec17" testExec17,
        TestLabel "testExec18" testExec18,
        TestLabel "testExec19" testExec19,
        TestLabel "testExec20" testExec20,
        TestLabel "testExec21" testExec21,
        TestLabel "testExec22" testExec22,
        TestLabel "testExec23" testExec23,
        TestLabel "testExec24" testExec24,
        TestLabel "testExec25" testExec25,
        TestLabel "testExec26" testExec26,
        TestLabel "testExec27" testExec27,
        TestLabel "testExec28" testExec28,
        TestLabel "testExec29" testExec29,
        TestLabel "testExec30" testExec30,
        TestLabel "testExec31" testExec31,
        TestLabel "testExec32" testExec32,
        TestLabel "testExec33" testExec33,
        TestLabel "testExec34" testExec34,
        TestLabel "testExec35" testExec35,
        TestLabel "testExec36" testExec36,
        TestLabel "testTakeListEnd1" testTakeListEnd1,
        TestLabel "testTakeListEnd2" testTakeListEnd2,
        TestLabel "testExec37" testExec37,
        TestLabel "testExec38" testExec38,
        TestLabel "testExec39" testExec39,
        TestLabel "testExec40" testExec40,
        TestLabel "testExec41" testExec41,
        TestLabel "testExec42" testExec42,
        TestLabel "testExec43" testExec43
    ]
