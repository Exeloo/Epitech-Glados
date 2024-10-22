{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- SExpToAst
-}

module SExprToAst (sExpToAst, evaluateAst) where

import AstData
import SExprData
import Symbol
import AstEval (evalAST)

type AstResult = Either String Ast

parseUnamedInstruction :: AstDeclaration -> [SExpr] -> AstResult
parseUnamedInstruction declaration args =
  case sExpElementsToAst args of
    Left err -> Left err
    Right (AList params) -> Right (ACall FuncCall {callFunction = FFunc declaration, callArgs = params})
    _ -> Left "Internal Error"

sExpListToAst :: [SExpr] -> AstResult
sExpListToAst [] = Right (AList [])
sExpListToAst (x:xs) =
  case sExpElementToAst x of
    Left err -> Left err
    Right (ASymbol sym) -> parseInstruction sym xs
    Right (ADeclaration declaration) -> parseUnamedInstruction declaration xs
    Right r -> case sExpElementsToAst xs of
      Left err -> Left err
      Right (AList rs) -> Right (AList (r:rs))
      _ -> Left "Internal Error"

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

parseLambda :: [SExpr] -> AstResult
parseLambda ((SList (args)):var:[]) =
  case parseFuncArgs args of
    Left err -> Left err
    Right params -> case sExpElementToAst var of
      Left err -> Left err
      Right value -> Right (ADeclaration FuncDeclaration {declareArgs = params, declareBody = [value]})
parseLambda _ = Left "Invalid lisp: lambda takes 2 params"

parseInstruction :: Symbol -> [SExpr] -> AstResult
parseInstruction "define" xs = parseDefine xs
parseInstruction "lambda" xs = parseLambda xs
parseInstruction sym [] = Right (ASymbol sym)
parseInstruction sym xs =
  case sExpElementsToAst xs of
    Left err -> Left err
    Right (AList elements) -> Right (ACall FuncCall {callFunction = FSymbol sym, callArgs = elements})
    _ -> Left "Internal Error"

sExpInstructionToAst :: [SExpr] -> AstResult
sExpInstructionToAst (x:xs) =
  case sExpElementToAst x of
    Left err -> Left err
    Right (ASymbol sym) -> parseInstruction sym xs
    Right (ADeclaration declaration) -> parseUnamedInstruction declaration xs
    _ -> Left "Invalid lisp: the first element must be an instruction"
sExpInstructionToAst _ = Left "Invalid lisp: the first element must be an instruction"

sExpToAst :: SExpr -> AstResult
sExpToAst (SList xs) = case sExpInstructionToAst xs of
  Left err -> Left err
  Right ast -> evaluateAst ast
sExpToAst _ = Left "Invalid lisp (first value must be an array)"

evaluateAst :: Ast -> [[Ast]] -> Either String [[Ast]]
evaluateAst ast scope = case evalAST scope ast of
    Left err -> Left err
    Right (ACall FuncCall {callFunction = declaration, callArgs = args}) -> evaluateAst (ACall FuncCall {callFunction = declaration, callArgs = args}) scope
    Right result -> Right result
