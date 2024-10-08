module UnitTests.EvalUnitTests (testlistEval) where

import Test.HUnit
import Evaluation
import AstData

testCheckElemList1 :: Test
testCheckElemList1 = TestCase (assertEqual "test" (Left "Not existing") (checkElemList ("test") []))

testCheckElemList2 :: Test
testCheckElemList2 = TestCase (assertEqual "test" (Left "Not existing") (checkElemList ("test") [[AAssignation (VarAssignation {assignationKey = "a", assignationValue = ASymbol "x"}), (AAssignation (VarAssignation {assignationKey = "b",  assignationValue = ASymbol "x"}))]]))

testCheckElemList3:: Test
testCheckElemList3 = TestCase (assertEqual "test" (Right True) (checkElemList ("test") [[AAssignation (VarAssignation {assignationKey = "a", assignationValue = ASymbol "x"}), (AAssignation (VarAssignation {assignationKey = "b",  assignationValue = ASymbol "x"}))], [AAssignation (VarAssignation {assignationKey = "foo", assignationValue = ASymbol "x"}), (AAssignation (VarAssignation {assignationKey = "test",  assignationValue = ASymbol "x"}))]]))

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
testgetElemList2 = TestCase (assertEqual "test not existing value" (Left "No useful data") (getElemList "a" [[AAssignation (VarAssignation {assignationKey = "z", assignationValue = ASymbol "x"})], [AAssignation (VarAssignation {assignationKey = "b", assignationValue = ASymbol "x"}), AAssignation (VarAssignation {assignationKey = "c", assignationValue = ASymbol "x"})]]))

testgetElemList3 :: Test
testgetElemList3 = TestCase (assertEqual "test empty list" (Left "No useful data") (getElemList "a" []))

testgetElemList4 :: Test
testgetElemList4 = TestCase (assertEqual "test empty list again" (Left "No useful data") (getElemList "a" [[], [], [], []]))

testaddAssignation1 :: Test
testaddAssignation1 = TestCase (assertEqual "test adding element empty list" ([[(AAssignation (VarAssignation {assignationKey = "a", assignationValue = ASymbol "x"}))]]) (addAssignation (AAssignation (VarAssignation {assignationKey = "a", assignationValue = ASymbol "x"})) []))

testlistEval :: Test
testlistEval = TestList [
    TestLabel "testCheckElemList1" testCheckElemList1,
    TestLabel "testCheckElemList2" testCheckElemList2,
    TestLabel "testCheckElemList3" testCheckElemList3,
    TestLabel "testfindAssignation1" testfindAssignation1,
    TestLabel "testfindAssignation2" testfindAssignation2,
    TestLabel "testfindAssignation3" testfindAssignation3,
    TestLabel "testfindAssignation4" testfindAssignation4,
    TestLabel "testgetElemList1" testgetElemList1,
    TestLabel "testgetElemList2" testgetElemList2,
    TestLabel "testgetElemList3" testgetElemList3,
    TestLabel "testgetElemList4" testgetElemList4,
    TestLabel "testaddAssignation1" testaddAssignation1
    ]
