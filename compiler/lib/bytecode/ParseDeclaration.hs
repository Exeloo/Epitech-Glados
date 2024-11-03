{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- ParseDeclaration
-}

module ParseDeclaration (parseDeclaration) where

import AstData
import BytecodeOperations
import BytecodeTypes

import ScopeUtils (hasVarInScopes, addVarToScope)

parseDeclaration :: AstDeclaration -> String -> BParams -> PBResult
parseDeclaration dec name params =
  if hasVarInScopes name params
  then (nParams, Right "")
  else (params, Left ("Invalid declaration name: \"" ++ name ++ "\" already exist."))
    where
      nParams = addVarToScope (name, (ADeclaration dec)) params
