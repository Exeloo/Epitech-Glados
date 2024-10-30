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
sExpVarAssignationToAst (SSymbol name: SSymbol "=" : value : xs) = case sExpInstructionToAst [value] of
  Right valueAst -> Right $ AAssignation (VarAssignation {
      assignationKey = name,
      assignationValue = valueAst
    })
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

sExpWhileToAst :: [SExpr] -> Either String Ast
sExpWhileToAst (cond: body: xs) = case sExpInstructionToAst [cond] of
  Right condAst -> case sExpInstructionToAst [body] of
    Right bodyAst -> Right $ ALoop $ WhileLoop {
      whileCondition = condAst,
      whileBody = [bodyAst]
    }
    Left err -> Left err
  Left err -> Left err
sExpWhileToAst x = Left $ "Invalid while: " ++ show x

sExpForToAst :: [SExpr] -> Either String Ast
sExpForToAst (SParenthesis [init, cond, inc]: SBracket [SLine body]:_) = case sExpInstructionToAst [init] of
  Right initAst -> case sExpInstructionToAst [cond] of
    Right condAst -> case sExpInstructionToAst [inc] of
      Right incAst -> case sExpInstructionToAst [body] of
        Right bodyAst -> Right $ ALoop $ ForLoop {
          forAssignation = [initAst],
          forCondition = [condAst],
          forIncrementation = [incAst],
          forBody = [bodyAst]
        }
        Left err -> Left err
      Left err -> Left err
    Left err -> Left err
  Left err -> Left err
sExpForToAst x = Left $ "Invalid for: " ++ show x

sExpInstructionToAst :: [SExpr] -> Either String Ast
sExpInstructionToAst (SSymbol "function": xs) = sExpFunctionToAst xs
sExpInstructionToAst (SSymbol "let": xs) = sExpVarAssignationToAst xs
sExpInstructionToAst (SSymbol "return": xs) = sExpInstructionToAst xs
sExpInstructionToAst (arg1: SSymbol x: arg2:_) | x `elem` ["+", "-", "*", "/", "%", "==", "<"] = sExpBuilinFunctionToAst x [arg1, arg2]
sExpInstructionToAst (SSymbol "if": cond: body: _) = sExpIfToAst [cond, body]
sExpInstructionToAst (SSymbol "while": SParenthesis [cond]: SBracket [SLine body]:_) = sExpWhileToAst [cond, body]
sExpInstructionToAst (SSymbol "for": xs) = sExpForToAst xs
sExpInstructionToAst (SInt x:_) = Right $ AInt x
sExpInstructionToAst (SBool x:_) = Right $ ABool x
sExpInstructionToAst (SFloat x:_) = Right $ AFloat x
sExpInstructionToAst (SSymbol x:_) = Right $ ASymbol x
sExpInstructionToAst (SString x:_) = Right $ AString x
sExpInstructionToAst (SArray x:_) = case mapM sExpInstructionToAst [x] of
  Right asts -> Right $ AList asts
  Left err -> Left err
sExpInstructionToAst (SParenthesis x:_) = mapM sExpInstructionToAst [x] >>= \asts -> Right $ AList asts
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
