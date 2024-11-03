{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- AstToBytecode
-}

module AstToBytecode (
  astToBytecode,
  parseBlock
) where

import AstData
import BytcodeOperations
import BytecodeTypes

import ParseAssignation (parseAssignation)
import ParseCall (parseCall)
import ParseLoop (parseLoop)


astToBytecode :: Ast -> BytecodeResult
astToBytecode ALine x = r
  where
    (_, r) = parseBlock x
astToBytecode _ = Left "Invalid code: The ast must start with a line"

parseBlock :: [Ast] -> BytecodeParams -> PBResult
parseBlock [] p = (p, Right "")
parseBlock (x:xs) = (rParams, (++) <$> line <*> r)
  where
    (tParams, line) = parseLine x
    (rParams, r) = parseBlock xs tParams

parseLine :: Ast -> BytecodeParams -> PBResult
parseLine (AAssignation x) p = parseAssignation x p
parseLine (ACall (FuncCall x)) p = parseCall (FuncCall x) p
parseLine (ALoop x) p = parseLoop x p
parseLine x _ = Left "Invalid line: " ++ show x
