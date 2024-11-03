module UnitTests.UnitTestsParser(testListParser) where

import Test.HUnit
import Parser
import Text.Megaparsec
import SExprData


testParseSExpr :: Test
testParseSExpr = TestCase $ assertEqual "test simple parse" (Right [SInstruction SCall]) (parse parseSExpr "" "Call")
testParseSExpr2 :: Test
testParseSExpr2 = TestCase $ assertEqual "test simple parse" (Right [SInstruction SPushStackOnArg]) (parse parseSExpr "" "PushStackOnArg")
testParseSExpr3 :: Test
testParseSExpr3 = TestCase $ assertEqual "test simple parse" (Right [SInstruction (SJump "here")]) (parse parseSExpr "" "Jump here")
testParseSExpr4 :: Test
testParseSExpr4 = TestCase $ assertEqual "test simple parse" (Right [SInstruction (SJumpIfFalse "+")]) (parse parseSExpr "" "JumpIfFalse +")


testParseSExpr5 :: Test
testParseSExpr5 = TestCase $ assertEqual "test simple parse" (Right (SInt 5)) (parse parseSValue "" "5")
testParseSExpr6 :: Test
testParseSExpr6 = TestCase $ assertEqual "test simple parse" (Right (SBool True)) (parse parseSValue "" "True")
testParseSExpr7 :: Test
testParseSExpr7 = TestCase $ assertEqual "test simple parse" (Right (SString "here")) (parse parseSValue "" "\"here\"")
testParseSExpr8 :: Test
testParseSExpr8 = TestCase $ assertEqual "test simple parse" (Right (SArray [SString "here", SInt 5])) (parse parseSValue "" "[\"here\",5]")
testParseSExpr9 :: Test
testParseSExpr9 = TestCase $ assertEqual "test simple parse" (Right SUndefined) (parse parseSValue "" "undefined")
testParseSExpr10 :: Test
testParseSExpr10 = TestCase $ assertEqual "test simple parse" (Right (SValueCall SAdd)) (parse parseSValue "" "Add")
testParseSExpr11 :: Test
testParseSExpr11 = TestCase $ assertEqual "test simple parse" (Right (SObject [("here", SInt 5)])) (parse parseSValue "" "{here : 5}")


testListParser :: Test
testListParser =
    TestList [
        TestLabel "testParseSExpr" testParseSExpr,
        TestLabel "testParseSExpr2" testParseSExpr2,
        TestLabel "testParseSExpr3" testParseSExpr3,
        TestLabel "testParseSExpr4" testParseSExpr4,
        TestLabel "testParseSExpr5" testParseSExpr5,
        TestLabel "testParseSExpr6" testParseSExpr6,
        TestLabel "testParseSExpr7" testParseSExpr7,
        TestLabel "testParseSExpr8" testParseSExpr8,
        TestLabel "testParseSExpr9" testParseSExpr9,
        TestLabel "testParseSExpr10" testParseSExpr10,
        TestLabel "testParseSExpr11" testParseSExpr11
    ]
