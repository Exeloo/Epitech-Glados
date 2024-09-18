import Test.HUnit

testSum = TestCase $ assertEqual "10 + 5 = 15" 15 (10 + 5)

testlist :: Test
testlist = TestList [TestLabel "testSum" testSum]

main :: IO ()
main = do
    runTestTT testlist
    return ()
