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

sExpFunctionToAst :: [SExpr] -> Either String Ast
sExpFunctionToAst (SSymbol name : SParenthesis args : SBracket [SLine body] : xs) =
  let argSymbols = map (\(SSymbol x) -> x) args
  in case sExpToAst (SLine body) of
       Right bodyAst -> Right $ AAssignation $ VarAssignation
         { assignationKey = name
         , assignationValue = ADeclaration $ FuncDeclaration
           { declareArgs = argSymbols
           , declareBody = [bodyAst]
           }
         }
       Left err -> Left err
sExpFunctionToAst _ = Left "Invalid function"

sExpVarAssignationToAst :: [SExpr] -> Either String Ast
sExpVarAssignationToAst (SSymbol name : value : xs) = case sExpInstructionToAst [value] of
  Right valueAst -> case sExpVarAssignationToAst xs of
    Right (AList rest) -> Right $ AList (AAssignation (VarAssignation {
      assignationKey = name,
      assignationValue = valueAst
    }) : rest)
    Right rest -> Right $ AList [AAssignation (VarAssignation {
      assignationKey = name,
      assignationValue = valueAst
    }), rest]
    Left err -> Left err
  Left err -> Left err
sExpVarAssignationToAst [] = Right $ AList []
sExpVarAssignationToAst x = Left $ "Invalid assignation: " ++ show x

sExpBuilinFunctionToAst :: String -> [SExpr] -> Either String Ast
sExpBuilinFunctionToAst opp [arg1, arg2] = sExpInstructionToAst [arg1] >>= \arg1Ast -> sExpInstructionToAst [arg2] >>= \arg2Ast -> Right $ ACall FuncCall {
  callFunction = ASymbol opp,
  callArgs = [arg1Ast, arg2Ast]
}
sExpBuilinFunctionToAst a b = Left $ "Invalid builtin function: " ++ a ++ " with args: " ++ show b

sExpIfToAst :: [SExpr] -> Either String Ast
sExpIfToAst (cond: body: xs) = case sExpInstructionToAst [cond] of
  Right condAst -> case sExpInstructionToAst [body] of
    Right bodyAst -> Right $ ACall FuncCall {
      callFunction = ASymbol "if",
      callArgs = [condAst, bodyAst]
    }
    Left err -> Left err
  Left err -> Left err

sExpInstructionToAst :: [SExpr] -> Either String Ast
sExpInstructionToAst (SSymbol "function": xs) = sExpFunctionToAst xs
sExpInstructionToAst (SSymbol "let": xs) = sExpVarAssignationToAst xs
sExpInstructionToAst (SSymbol "return": xs) = sExpInstructionToAst xs
sExpInstructionToAst (arg1: SSymbol x: arg2:xs) | x `elem` ["+", "-", "*", "/", "%", "==", "<"] = sExpBuilinFunctionToAst x [arg1, arg2]
sExpInstructionToAst (SSymbol "if": cond: body: xs) = sExpIfToAst [cond, body]
sExpInstructionToAst (SInt x:_) = Right $ AInt x
sExpInstructionToAst (SBool x:_) = Right $ ABool x
sExpInstructionToAst (SFloat x:_) = Right $ AFloat x
sExpInstructionToAst (SSymbol x:_) = Right $ ASymbol x
sExpInstructionToAst (SString x:_) = Right $ AString x
sExpInstructionToAst (SArray x:_) = case mapM sExpInstructionToAst [x] of
  Right asts -> Right $ AList asts
  Left err -> Left err
sExpInstructionToAst (SParenthesis x:_) = mapM sExpInstructionToAst [x] >>= \asts -> Right $ AList asts
sExpInstructionToAst (SStruct x:_) = mapM sExpInstructionToAst x >>= \asts -> Right $ AObject $ map (\ast -> ObjectElement {
  objectKey = "key",
  objectValue = ast
}) asts
sExpInstructionToAst (SLine x:xs) = case sExpInstructionToAst x of
  Right ast | xs == [] -> Right ast
            | otherwise -> case mapM sExpInstructionToAst [xs] of
    Right asts -> Right $ ALine (ast:asts)
    Left err -> Left err
  Left err -> Left err

sExpInstructionToAst x = Left $ "Invalid instruction: " ++ show x

sExpToAst :: SExpr -> Either String Ast
sExpToAst (SLine xs) = sExpInstructionToAst xs
sExpToAst x = Left $ "Invalid SExpr: " ++ show x
