{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- StackUtils
-}

module StackUtils (pushArgOnStack, invalidValueOnStack) where

import AstData
import BytecodeOperations
import BytecodeTypes

import ScopeUtils (getArgIndexFromScopes)

invalidValueOnStack :: Ast -> String
invalidValueOnStack v = "Invalid value on stack : trying to push " ++ show v ++ " on stack."

pushArgOnStack :: String -> BParams -> BResult
pushArgOnStack arg (_, (args, _), _, _) =
  if index /= -1
  then Right (getPushArgOnStack index)
  else Left ("Unknown argument : this argument \"" ++ arg ++ "\" does not exist. Please check if it is declared in the right scope.")
    where
      index = getArgIndexFromScopes arg args
