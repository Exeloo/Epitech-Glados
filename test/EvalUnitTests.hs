module EvalUnitTests where

import Test.HUnit
import Evaluation
import AstData
import Symbol

testCheckElemList1 :: Test
testCheckElemList1 = TestCase (assertEqual "test" (Left "Not existing") (checkElemList ("test") []))

testCheckElemList2 :: Test
testCheckElemList2 = TestCase (assertEqual "test" (Left "Not existing") (checkElemList ("test") [[AAssignation (VarAssignation {assignationKey = "a", assignationValue = ASymbol "x"}), (AAssignation (VarAssignation {assignationKey = "b",  assignationValue = ASymbol "x"}))]]))

testCheckElemList3:: Test
testCheckElemList3 = TestCase (assertEqual "test" (Right True) (checkElemList ("test") [[AAssignation (VarAssignation {assignationKey = "a", assignationValue = ASymbol "x"}), (AAssignation (VarAssignation {assignationKey = "b",  assignationValue = ASymbol "x"}))], [AAssignation (VarAssignation {assignationKey = "foo", assignationValue = ASymbol "x"}), (AAssignation (VarAssignation {assignationKey = "test",  assignationValue = ASymbol "x"}))]]))

getAssignationKeyList1 :: Test
getAssignationKeyList1 = TestCase (assertEqual "test" (["a", "b"]) (getAssignationKeyList [AAssignation (VarAssignation {assignationKey = "a", assignationValue = ASymbol "x"}), (AAssignation (VarAssignation {assignationKey = "b",  assignationValue = ASymbol "x"}))]))

getAssignationKeyList2 :: Test
getAssignationKeyList2 = TestCase (assertEqual "test" ([]) (getAssignationKeyList [ASymbol "foo"]))
