module UnitTests.UnitTestsSExprToInstruction(testListSExprToInstruction) where

import Test.HUnit
import SExprData
import InstructionData
import SExprToInstructions

testMapSArrayToVArray :: Test
testMapSArrayToVArray = TestCase $ assertEqual "check if working" ([(0, (VString "A")), (1, (VString "B")), (2, (VString "C"))]) (mapSArrayToVArray [(SString "A"), (SString "B"), (SString "C")] 0)


testSValToValueData1 :: Test
testSValToValueData1 = TestCase $ assertEqual "check result for int" (VInt 5) (sValToValueData (SInt 5))
testSValToValueData2 :: Test
testSValToValueData2 = TestCase $ assertEqual "check result for bool" (VBool True) (sValToValueData (SBool True))
testSValToValueData3 :: Test
testSValToValueData3 = TestCase $ assertEqual "check result for double" (VDouble 42) (sValToValueData (SDouble 42))
testSValToValueData4 :: Test
testSValToValueData4 = TestCase $ assertEqual "check result for array" (VArray [(0,(VInt 5))]) (sValToValueData (SArray [(SInt 5)]))
testSValToValueData5 :: Test
testSValToValueData5 = TestCase $ assertEqual "check result for object" (VObject [("here", VInt 5)]) (sValToValueData (SObject [("here", (SInt 5))]))
testSValToValueData6 :: Test
testSValToValueData6 = TestCase $ assertEqual "check result for add" (VCall Add) (sValToValueData (SValueCall SAdd))
testSValToValueData7 :: Test
testSValToValueData7 = TestCase $ assertEqual "check result for sub" (VCall Sub) (sValToValueData (SValueCall SSub))
testSValToValueData8 :: Test
testSValToValueData8 = TestCase $ assertEqual "check result for mul" (VCall Mul) (sValToValueData (SValueCall SMul))
testSValToValueData9 :: Test
testSValToValueData9 = TestCase $ assertEqual "check result for div" (VCall Div) (sValToValueData (SValueCall SDiv))
testSValToValueData10 :: Test
testSValToValueData10 = TestCase $ assertEqual "check result for eq" (VCall Eq) (sValToValueData (SValueCall SEq))
testSValToValueData11 :: Test
testSValToValueData11 = TestCase $ assertEqual "check result for less" (VCall Less) (sValToValueData (SValueCall SLess))
testSValToValueData12 :: Test
testSValToValueData12 = TestCase $ assertEqual "check result for not" (VCall Not) (sValToValueData (SValueCall SNot))
testSValToValueData13 :: Test
testSValToValueData13 = TestCase $ assertEqual "check result for or" (VCall Or) (sValToValueData (SValueCall SOr))
testSValToValueData14 :: Test
testSValToValueData14 = TestCase $ assertEqual "check result for not" (VCall And) (sValToValueData (SValueCall SAnd))
testSValToValueData15 :: Test
testSValToValueData15 = TestCase $ assertEqual "check result for acces array" (VCall AccessArray) (sValToValueData (SValueCall SAccessArray))
testSValToValueData16 :: Test
testSValToValueData16 = TestCase $ assertEqual "check result for modify array" (VCall ModifyArray) (sValToValueData (SValueCall SModifyArray))
testSValToValueData17 :: Test
testSValToValueData17 = TestCase $ assertEqual "check result for acces object" (VCall AccessObject) (sValToValueData (SValueCall SAccessObject))
testSValToValueData18 :: Test
testSValToValueData18 = TestCase $ assertEqual "check result for acces object" (VCall ModifyObject) (sValToValueData (SValueCall SModifyObject))
testSValToValueData19 :: Test
testSValToValueData19 = TestCase $ assertEqual "check result for acces object" (VCall Print) (sValToValueData (SValueCall SPrint))
testSValToValueData20 :: Test
testSValToValueData20 = TestCase $ assertEqual "check result for acces object" (VUndefined) (sValToValueData (SUndefined))


testGetLabelIndexInInst1 :: Test
testGetLabelIndexInInst1 = TestCase $ assertEqual "check result for a SInstruction" (Nothing) (getLabelIndexInInst ([(SInstruction (SPushOnStack (SInt 5)))]) "here")
testGetLabelIndexInInst2 :: Test
testGetLabelIndexInInst2 = TestCase $ assertEqual "check result for int" (Just 0) (getLabelIndexInInst ([(SLabel "test"), (SLabel "here")]) "here")


testSInstToInst1 :: Test
testSInstToInst1 = TestCase $ assertEqual "check result for push on stack" (Right (Push (VInt 5))) (sInstToInst ([(SLabel "here")]) (SPushOnStack (SInt 5)))
testSInstToInst2 :: Test
testSInstToInst2 = TestCase $ assertEqual "check result for sCall" (Right Call) (sInstToInst ([(SLabel "here")]) (SCall))
testSInstToInst3 :: Test
testSInstToInst3 = TestCase $ assertEqual "check result for sRet" (Right Ret) (sInstToInst ([(SLabel "here")]) (SRet))
testSInstToInst4 :: Test
testSInstToInst4 = TestCase $ assertEqual "check result for Push arg on stack" (Right (PushArgOnStack 5)) (sInstToInst ([(SLabel "here")]) (SPushArgOnStack 5))
testSInstToInst5 :: Test
testSInstToInst5 = TestCase $ assertEqual "check result for pop arg" (Right (PopArg)) (sInstToInst ([(SLabel "here")]) (SPopArg))
testSInstToInst6 :: Test
testSInstToInst6 = TestCase $ assertEqual "check result for pop stack" (Right (PopStack)) (sInstToInst ([(SLabel "here")]) (SPopStack))
testSInstToInst7 :: Test
testSInstToInst7 = TestCase $ assertEqual "check result for jump" (Right (Jump 0)) (sInstToInst ([(SLabel "here")]) (SJump "here"))
testSInstToInst8 :: Test
testSInstToInst8 = TestCase $ assertEqual "check result for jump" (Left ("Jump label not found: test")) (sInstToInst ([(SLabel "here")]) (SJump "test"))
testSInstToInst9 :: Test
testSInstToInst9 = TestCase $ assertEqual "check result for jump if false" (Right (JumpIfFalse 0)) (sInstToInst ([(SLabel "here")]) (SJumpIfFalse "here"))
testSInstToInst10 :: Test
testSInstToInst10 = TestCase $ assertEqual "check result for jump if false" (Left ("JumpIfFalse label not found: test")) (sInstToInst ([(SLabel "here")]) (SJumpIfFalse"test"))
testSInstToInst11 :: Test
testSInstToInst11 = TestCase $ assertEqual "check result for Push stack on arg" (Right (PushStackOnArg)) (sInstToInst ([(SLabel "here")]) (SPushStackOnArg))


testSExprToInsts1 :: Test
testSExprToInsts1 = TestCase $ assertEqual "check result for int" (Right [Call]) (sExprToInsts [(SLabel "here"), (SInstruction (SCall))])


testListSExprToInstruction :: Test
testListSExprToInstruction =
    TestList [
        TestLabel "testMapSArrayToVArray" testMapSArrayToVArray,
        TestLabel "testSValToValueData1" testSValToValueData1,
        TestLabel "testSValToValueData2" testSValToValueData2,
        TestLabel "testSValToValueData3" testSValToValueData3,
        TestLabel "testSValToValueData4" testSValToValueData4,
        TestLabel "testSValToValueData5" testSValToValueData5,
        TestLabel "testSValToValueData6" testSValToValueData6,
        TestLabel "testSValToValueData7" testSValToValueData7,
        TestLabel "testSValToValueData8" testSValToValueData8,
        TestLabel "testSValToValueData9" testSValToValueData9,
        TestLabel "testSValToValueData10" testSValToValueData10,
        TestLabel "testSValToValueData11" testSValToValueData11,
        TestLabel "testSValToValueData12" testSValToValueData12,
        TestLabel "testSValToValueData13" testSValToValueData13,
        TestLabel "testSValToValueData14" testSValToValueData14,
        TestLabel "testSValToValueData15" testSValToValueData15,
        TestLabel "testSValToValueData16" testSValToValueData16,
        TestLabel "testSValToValueData17" testSValToValueData17,
        TestLabel "testSValToValueData18" testSValToValueData18,
        TestLabel "testSValToValueData19" testSValToValueData19,
        TestLabel "testSValToValueData20" testSValToValueData20,
        TestLabel "testGetLabelIndexInInst1" testGetLabelIndexInInst1,
        TestLabel "testGetLabelIndexInInst2" testGetLabelIndexInInst2,
        TestLabel "testSInstToInst1" testSInstToInst1,
        TestLabel "testSInstToInst2" testSInstToInst2,
        TestLabel "testSInstToInst3" testSInstToInst3,
        TestLabel "testSInstToInst4" testSInstToInst4,
        TestLabel "testSInstToInst5" testSInstToInst5,
        TestLabel "testSInstToInst6" testSInstToInst6,
        TestLabel "testSInstToInst7" testSInstToInst7,
        TestLabel "testSInstToInst8" testSInstToInst8,
        TestLabel "testSInstToInst9" testSInstToInst9,
        TestLabel "testSInstToInst10" testSInstToInst10,
        TestLabel "testSInstToInst11" testSInstToInst11,
        TestLabel "testSExprToInsts1" testSExprToInsts1
    ]
