{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- SExpToAst
-}

module SExprToAst (sExpToAst) where

import AstData
import SExprData
import Symbol

type AstResult = Either String Ast

sExpListToAst :: [SExpr] -> AstResult
sExpListToAst ((SSymbol sym):xs) = parseInstruction sym xs
sExpListToAst arr = sExpElementsToAst arr

sExpElementToAst :: SExpr -> AstResult
sExpElementToAst (SInt x) = Right (AInt x)
sExpElementToAst (SString x) = Right (AString x)
sExpElementToAst (SSymbol x) = Right (ASymbol x)
sExpElementToAst (SList xs) = sExpListToAst xs

sExpElementsToAst :: [SExpr] -> AstResult
sExpElementsToAst [] = Right (AList [])
sExpElementsToAst (x:xs) =
  case sExpElementToAst x of
    Left err -> Left err
    Right curr -> case sExpElementsToAst xs of
      Right (AList nexts) -> Right (AList (curr:nexts))
      Left err -> Left err
      _ -> Left "Internal Error"

parseFuncArgs :: [SExpr] -> Either String [Symbol]
parseFuncArgs [] = Right []
parseFuncArgs ((SSymbol sym):xs) =
  case parseFuncArgs xs of
    Right nexts -> Right (sym:nexts)
    Left err -> Left err
parseFuncArgs _ = Left "Invalid functions params"

parseDefine :: [SExpr] -> AstResult
parseDefine ((SSymbol sym):var:[]) =
  case sExpElementToAst var of
    Left err -> Left err
    Right value -> Right (AAssignation VarAssignation {assignationKey = sym, assignationValue = value})
parseDefine ((SList ((SSymbol sym):args)):var:[]) =
  case parseFuncArgs args of
    Left err -> Left err
    Right params -> case sExpElementToAst var of
      Left err -> Left err
      Right value -> Right (AAssignation VarAssignation {assignationKey = sym, assignationValue = (ADeclaration FuncDeclaration {declareArgs = params, declareBody = [value]})})
parseDefine _ = Left "Invalid lisp: define takes 2 params"

--parseLambda :: [SExpr] -> AstResult

parseInstruction :: Symbol -> [SExpr] -> AstResult
parseInstruction "define" xs = parseDefine xs
--parseInstruction "lambda" xs = parseLambda xs
parseInstruction sym xs =
  case sExpElementsToAst xs of
    Left err -> Left err
    Right (AList elements) -> Right (ACall FuncCall {callFunction = FSymbol sym, callArgs = elements})
    _ -> Left "Internal Error"

sExpInstructionToAst :: [SExpr] -> AstResult
sExpInstructionToAst ((SSymbol sym):xs) = parseInstruction sym xs
sExpInstructionToAst _ = Left "Invalid lisp: the first value of an instruction must be a symbol"


sExpToAst :: SExpr -> AstResult
sExpToAst (SList xs) = sExpInstructionToAst xs
sExpToAst _ = Left "Invalid lisp (first value must be an array)"
