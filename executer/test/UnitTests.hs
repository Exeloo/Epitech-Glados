import Test.HUnit (runTestTT)
import UnitTests.UnitTestsSExprToInstruction

main :: IO ()
main = do
    _ <- runTestTT testListSExprToInstruction
    return ()
