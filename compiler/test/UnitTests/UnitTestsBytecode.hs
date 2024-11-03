module UnitTests.UnitTestsBytecode(testListBytecode) where

import Test.HUnit
import AstToBytecode
import AstData

testsastToBytecodeAssignation :: Test
testsastToBytecodeAssignation = TestCase $ assertEqual "test astToBytecode assignation" (Right "Push 5\nPushStackOnArg\n") (astToBytecode (ALine[AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = AInt 5 }]))

testsastToBytecodeAssignationError :: Test
testsastToBytecodeAssignationError = TestCase $ assertEqual "test astToBytecode assignation error" (Left "Unknown argument : this argument \"bar\" does not exist. Please check if it is declared in the right scope.") (astToBytecode (ALine[AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = ASymbol "bar" }]))

testsastToBytecodeForLoop :: Test
testsastToBytecodeForLoop = TestCase $ assertEqual "test astToBytecode for loop" (Right "Push 5\nPushStackOnArg\n.while_0_in:\nPush true\nJumpIfFalse while_0_out\nPopArg\nPush 5\nPushStackOnArg\nPushStackOnArg\nPopArg\nPush 5\nPushStackOnArg\nPushStackOnArg\nJump while_0_in\n.while_0_out:\nPopArg\n") (astToBytecode (ALine[ALoop (ForLoop { forAssignation = [AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = AInt 5 }], forCondition = ABool True, forIncrementation = [AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = AInt 5 }], forBody = ALine [AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = AInt 5 }]})]))

testsastToBytecodeForLoopError :: Test
testsastToBytecodeForLoopError = TestCase $ assertEqual "test astToBytecode for loop error" (Left "Unknown argument : this argument \"bar\" does not exist. Please check if it is declared in the right scope.") (astToBytecode (ALine[ALoop (ForLoop { forAssignation = [AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = AInt 5 }], forCondition = ABool True, forIncrementation = [AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = ASymbol "bar" }], forBody = ALine [AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = AInt 5 }]})]))

testsastToBytecodeWhileLoop :: Test
testsastToBytecodeWhileLoop = TestCase $ assertEqual "test astToBytecode while loop" (Right ".while_0_in:\nPush true\nJumpIfFalse while_0_out\nPush 5\nPushStackOnArg\nJump while_0_in\n.while_0_out:\nPopArg\n") (astToBytecode (ALine[ALoop (WhileLoop { whileCondition = ABool True, whileBody = ALine [AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = AInt 5 }]})]))

testsastToBytecodeWhileLoopError :: Test
testsastToBytecodeWhileLoopError = TestCase $ assertEqual "test astToBytecode while loop error" (Left "Unknown argument : this argument \"bar\" does not exist. Please check if it is declared in the right scope.") (astToBytecode (ALine[ALoop (WhileLoop { whileCondition = ASymbol "bar", whileBody = ALine [AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = AInt 5 }]})]))

testsastToBytecodeReAssignation :: Test
testsastToBytecodeReAssignation = TestCase $ assertEqual "test astToBytecode re-assignation" (Right "Push 5\nPushStackOnArg\nPopArg\nPush 6\nPushStackOnArg\nPushStackOnArg\n") (astToBytecode (ALine[AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = AInt 5 }, AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = AInt 6 }]))

testsastToBytecodeModifyingArray :: Test
testsastToBytecodeModifyingArray = TestCase $ assertEqual "test astToBytecode modifying array" (Right "Push [ 5 ]\nPushStackOnArg\nPushArgOnStack 0\nPush 0\nPush 6\nPush Modify\nCall\nPushStackOnArg\n") (astToBytecode (ALine [    AAssignation $ VarAssignation { assignationKey = "foo", assignationValue = AList [AInt 5] },    AAssignation $ AccessAssignation { assignationAccessArray = ASymbol "foo", assignationAccessArg = AInt 0, assignationAccessValue = AInt 6 }]))

testListBytecode :: Test
testListBytecode = TestList [
  testsastToBytecodeAssignation,
  testsastToBytecodeAssignationError,
  testsastToBytecodeForLoop,
  testsastToBytecodeForLoopError,
  testsastToBytecodeWhileLoop,
  testsastToBytecodeWhileLoopError,
  testsastToBytecodeReAssignation,
  testsastToBytecodeModifyingArray
  ]
