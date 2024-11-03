{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- StackUtils
-}

module StackUtils (pushValueOnStack) where

import AstData
import BytcodeOperations
import BytecodeTypes

import ScopeUtils (getArgIndexFromScope)

pushValueOnStack :: Ast -> BytecodeParams -> PBResult
pushValueOnStack (ASymbol sym) p = (p, pushArgOnStack sym p)
pushValueOnStack (ACall call) p = parseCall call p
pushValueOnStack (ALine x) p = (p, Left (invalidValueOnStack (ALine x)))
pushValueOnStack (ADeclaration x) p = (p, Left (invalidValueOnStack (ADeclaration x)))
pushValueOnStack (AAssignation x) p = (p, Left (invalidValueOnStack (AAssignation x)))
pushValueOnStack (ALoop x) p = (p, Left (invalidValueOnStack (ALoop x)))
pushValueOnStack value p = (p, getPush (show value))

invalidValueOnStack :: Ast -> String
invalidValueOnStack v = "Invalid value on stack : trying to push " ++ show value ++ " on stack."

pushArgOnStack :: String -> BytecodeParams -> BResult
pushArgOnStack arg (_, { bArgs = args }, _, _) =
  if index != -1
  then Right (getPushArgOnStack index)
  else Left "Unknown argument : this argument \"" ++ arg ++ "\" does not exist. Please check if it is declared in the right scope."
    where
      index = getArgIndexFromScope arg args
