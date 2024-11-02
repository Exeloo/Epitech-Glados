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
        TestLabel "testSValToValueData20" testSValToValueData20
    ]
