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

testfindAssignation1 :: Test
testfindAssignation1 = TestCase (assertEqual "test if working" (Just (AAssignation (VarAssignation {assignationKey = "a", assignationValue = ASymbol "x"}))) (findAssignation "a" [AAssignation (VarAssignation {assignationKey = "a", assignationValue = ASymbol "x"})]))

testfindAssignation2 :: Test
testfindAssignation2 = TestCase (assertEqual "test empty list" (Nothing) (findAssignation "a" []))

testfindAssignation3 :: Test
testfindAssignation3 = TestCase (assertEqual "test not existing value" (Nothing) (findAssignation "a" [AAssignation (VarAssignation {assignationKey = "b", assignationValue = ASymbol "x"}), AAssignation (VarAssignation {assignationKey = "c", assignationValue = ASymbol "x"})]))

testfindAssignation4 :: Test
testfindAssignation4 = TestCase (assertEqual "test invalid value in list" (Nothing) (findAssignation "a" [ASymbol "foo", ASymbol "faa"]))

testgetElemList1 :: Test
testgetElemList1 = TestCase (assertEqual "test if working" (Right (AAssignation (VarAssignation {assignationKey = "a", assignationValue = ASymbol "x"}))) (getElemList "a" [[AAssignation (VarAssignation {assignationKey = "a", assignationValue = ASymbol "x"})], [AAssignation (VarAssignation {assignationKey = "b", assignationValue = ASymbol "x"}), AAssignation (VarAssignation {assignationKey = "c", assignationValue = ASymbol "x"})]]))

testgetElemList2 :: Test
testgetElemList2 = TestCase (assertEqual "test not existing value" (Left "No usefull data") (getElemList "a" [[AAssignation (VarAssignation {assignationKey = "z", assignationValue = ASymbol "x"})], [AAssignation (VarAssignation {assignationKey = "b", assignationValue = ASymbol "x"}), AAssignation (VarAssignation {assignationKey = "c", assignationValue = ASymbol "x"})]]))

testgetElemList3 :: Test
testgetElemList3 = TestCase (assertEqual "test empty list" (Left "No usefull data") (getElemList "a" []))

testgetElemList4 :: Test
testgetElemList4 = TestCase (assertEqual "test empty list again" (Left "No usefull data") (getElemList "a" [[], [], [], []]))

testgetElemList5 :: Test
testgetElemList5 = TestCase (assertEqual "test getting from last scope" (Right (AAssignation (VarAssignation {assignationKey = "a", assignationValue = ASymbol "the true one"}))) (getElemList "a" [[AAssignation (VarAssignation {assignationKey = "a", assignationValue = ASymbol "the false one"})], [AAssignation (VarAssignation {assignationKey = "a", assignationValue = ASymbol "the true one"}), AAssignation (VarAssignation {assignationKey = "c", assignationValue = ASymbol "x"})]]))
