{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- BytecodeTypes
-}

module BytecodeTypes (
  BPath,
  BArgs,
  BVars,
  BLabels,
  BParams,
  BResult,
  PBResult
) where

import AstData
import Symbol (Symbol)

type BPath = [String]
type BArgs = ([[(Symbol, Int)]], Int)
type BVars = [[(Symbol, Ast)]]
type BLabels = [(String, Int)]
type BParams = (BPath, BArgs, BVars, BLabels)

type BResult = Either String String
type PBResult = (BParams, BResult)
