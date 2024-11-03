import Test.HUnit (runTestTT)
import UnitTests.UnitTestsSExprToInstruction
import UnitTests.UnitTestsExecInstruction
import UnitTests.UnitTestsInstructionData
import UnitTests.UnitTestsSExprData

main :: IO ()
main = do
    _ <- runTestTT testListSExprToInstruction
    _ <- runTestTT testListExecInstruction
    _ <- runTestTT testListInstructionData
    _ <- runTestTT testListSExprData
    return ()
