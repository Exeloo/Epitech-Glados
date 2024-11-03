{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- BytecodeOperations
-}

module BytecodeOperations (
  getLabel,
  getPush,
  getPushArgOnStack,
  getPushStackOnArg,
  getPopArg,
  getJump,
  getJumpIfFalse,
  getCall,
  getRet
) where

getLabel :: String -> String
getLabel x = "." ++ x ++ ":\n"

getPush :: String -> String
getPush x = "Push " ++ x ++ "\n"

getPushArgOnStack :: Int -> String
getPushArgOnStack x = "PushArgOnStack " ++ show x ++ "\n"

getPushStackOnArg :: String
getPushStackOnArg = "PushStackOnArg\n"

getPopArg :: String
getPopArg = "PopArg\n"

getJump :: String -> String
getJump x = "Jump " ++ x ++ "\n"

getJumpIfFalse :: String -> String
getJumpIfFalse x = "JumpIfFalse " ++ x ++ "\n"

getCall :: String
getCall = "Call\n"

getRet :: String
getRet = "Ret\n"
