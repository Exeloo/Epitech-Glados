module EvalUnitTests where

import Test.HUnit
import Evaluation
import AstData

testCheckElemList1 :: Test
testCheckElemList1 = TestCase (assertEqual "testest" (Left "Not existing") (checkElemList (ASymbol "test") []))

testCheckElemList2 :: Test
testCheckElemList2 = TestCase (assertEqual "testest" (Left "Not existing") (checkElemList (ASymbol "test") [[(ASymbol "a"), (ASymbol "b")]]))

testCheckElemList3:: Test
testCheckElemList3 = TestCase (assertEqual "testest" (Right True) (checkElemList (ASymbol "test") [[(ASymbol "a"), (ASymbol "b")], [(ASymbol "test")]]))
