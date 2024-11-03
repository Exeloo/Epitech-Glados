module UnitTests.UnitTestsInstructionData(testListInstructionData) where

import Test.HUnit
import InstructionData

testShowSysCall :: Test
testShowSysCall = TestCase $ assertEqual "test show a syscall" "Add" (show Add)

testShowInstructionData :: Test
testShowInstructionData = TestCase $ assertEqual "test show an instruction data" "Call" (show Call)
 
testEqInstructionData :: Test
testEqInstructionData = TestCase $ assertEqual "test show an instruction data" True (Ret == Ret)


testListInstructionData :: Test
testListInstructionData =
    TestList [
        TestLabel "testShowSysCall" testShowSysCall,
        TestLabel "testShowInstructionData" testShowInstructionData,
        TestLabel "testEqInstructionData" testEqInstructionData
    ]
