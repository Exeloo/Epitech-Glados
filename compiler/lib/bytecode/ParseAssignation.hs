{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- ParseAssignation
-}

module ParseAssignation (parseAssignation) where

import AstData
import BytcodeOperations
import BytecodeTypes

import ScopeUtils (hasArgInScopes, addArgToScope)
import StackUtils (pushValueOnStack)

import ParseCall (parseCall)
import ParseDeclaration (parseDeclaration)

parseAssignation :: AstAssignation -> BytecodeParams -> PBResult
parseAssignation (VarAssignation { assignationKey = key, assignationValue = (ADeclaration value) }) params = parseDeclaration value key params
parseAssignation (VarAssignation { assignationKey = key, assignationValue = value }) params =
  if hasArgInScopes key params
  then parseNewAssignation key value params
  else parseOldAssignation key value params

parseNewAssignation :: String -> Ast -> BytecodeParams -> PBResult
parseNewAssignation key value params = (n2Params, (++) <$> r1 <*> r2)
  where
    (n1Params, r1) = pushValueOnStack value params
    (n2Params, r2) = addArgToScope key n1Params

parseOldAssignation :: String -> Ast -> BytecodeParams -> PBResult
parseOldAssignation key value params = (n2Params, (++) <$> r1 <*> r2)
  where
    (n1Params, r1) = pushValueOnStack value params
    (n2Params, r2) = addArgToScope key n1Params
