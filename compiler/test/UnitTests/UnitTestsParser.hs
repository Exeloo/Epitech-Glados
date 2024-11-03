module UnitTests.UnitTestsParser(parserTestList) where

import Test.HUnit
import Text.Megaparsec
import Parser
import SExprData(SExpr(..))

-- assertLeft :: String -> Either a b -> Assertion
-- assertLeft _ (Left _) = return ()
-- assertLeft msg (Right _) = assertFailure $ msg ++ " -> expected failure, but got a Right value"

-- testSuccessParseSInt :: Test
-- testSuccessParseSInt = TestCase $ assertEqual "Parse int 42" (Right (SInt 42)) (parse parseSInt "" "42")
-- testErrorParseSInt :: Test
-- testErrorParseSInt = TestCase $ assertLeft "Error parsing int a42" (parse parseSInt "" "a42")

-- testSuccessParseSListDefineFct :: Test
-- testSuccessParseSListDefineFct = TestCase $ assertEqual "Parse list (define plus +)" (Right (SList [SSymbol "define", SSymbol "plus", SSymbol "+"])) (parse parseSList "" "(define plus +)")
-- testSuccessParseSListComplex :: Test
-- testSuccessParseSListComplex = TestCase $ assertEqual "Parse list (+ (+ 1 2) 1 (- 3 4 \"hihi\"))" (Right (SList [SSymbol "+", SList [SSymbol "+", SInt 1, SInt 2], SInt 1, SList [SSymbol "-", SInt 3, SInt 4, SString "hihi"]])) (parse parseSList "" "(+ (+ 1 2) 1 (- 3 4 \"hihi\"))")
-- testErrorParseSListWithQuote :: Test
-- testErrorParseSListWithQuote = TestCase $ assertLeft "Error parsing list \"abc\"" (parse parseSList "" "\"abc\"")
-- testErrorParseSListNoEnd :: Test
-- testErrorParseSListNoEnd = TestCase $ assertLeft "Error parsing list (define" (parse parseSList "" "(define")

-- testSuccessParseSSymbolAllChar :: Test
-- testSuccessParseSSymbolAllChar = TestCase $ assertEqual "Parse symbol <>+-_?Aa10" (Right (SSymbol "<")) (parse parseSSymbol "" "<>+-_?Aa10")
-- testSuccessParseSSymbolStartingNum :: Test
-- testSuccessParseSSymbolStartingNum = TestCase $ assertEqual "Parse symbol 11b" (Right (SSymbol "11b")) (parse parseSSymbol "" "11b")
-- testErrorParseSSymbolQuote :: Test
-- testErrorParseSSymbolQuote = TestCase $ assertLeft "Error parsing symbol \"abc\"" (parse parseSSymbol "" "\"abc\"")

-- testSuccessParseSStringDoubleQuote :: Test
-- testSuccessParseSStringDoubleQuote = TestCase $ assertEqual "Parse string \"abc\"" (Right (SString "abc")) (parse parseSString "" "\"abc\"")
-- testSuccessParseSStringSingleQuote :: Test
-- testSuccessParseSStringSingleQuote = TestCase $ assertEqual "Parse string \'abc\'" (Right (SString "abc")) (parse parseSString "" "\'abc\'")
-- testErrorNoQuotesParseSString :: Test
-- testErrorNoQuotesParseSString = TestCase $ assertLeft "Error parsing string abc" (parse parseSString "" "abc")

parserTestList :: Test
parserTestList = TestList [
    -- TestLabel "testSuccessParseSInt" testSuccessParseSInt,
    -- TestLabel "testErrorParseSInt" testErrorParseSInt,

    -- TestLabel "testSuccessParseSListDefineFct" testSuccessParseSListDefineFct,
    -- TestLabel "testSuccessParseSListComplex" testSuccessParseSListComplex,
    -- TestLabel "testErrorParseSListWithQuote" testErrorParseSListWithQuote,
    -- TestLabel "testErrorParseSListNoEnd" testErrorParseSListNoEnd,

    -- TestLabel "testSuccessParseSSymbolAllChar" testSuccessParseSSymbolAllChar,
    -- TestLabel "testSuccessParseSSymbolStartingNum" testSuccessParseSSymbolStartingNum,
    -- TestLabel "testErrorParseSSymbolQuote" testErrorParseSSymbolQuote,

    -- TestLabel "testSuccessParseSStringDoubleQuote" testSuccessParseSStringDoubleQuote,
    -- TestLabel "testSuccessParseSStringSingleQuote" testSuccessParseSStringSingleQuote,
    -- TestLabel "testErrorNoQuotesParseSString" testErrorNoQuotesParseSString
    ]
