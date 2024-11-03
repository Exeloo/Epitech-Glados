{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- SExpToAst
-}

module SExprToAst (sExpFunctionToAst, sExpVarAssignationToAst, sExpBuilinFunctionToAst, sExpIfToAst, sExpWhileToAst, sExpForToAst, sExpStructToAst, sExpInstructionToAst, sExpToAst) where

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
           , declareBody = bodyAst
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
      whileBody = bodyAst
    }
    Left err -> Left err
  Left err -> Left err
sExpWhileToAst x = Left $ "Invalid while: " ++ show x

sExpForToAst :: [SExpr] -> Either String Ast
sExpForToAst (SParenthesis args: SBracket [SLine body]:_) = sExpForToAstArgs args >>= \(init, cond, inc) -> case sExpInstructionToAst [SLine body] of
  Right bodyAst -> Right $ ALoop $ ForLoop {
    forAssignation = init,
    forCondition = cond,
    forIncrementation = inc,
    forBody = bodyAst
  }
sExpForToAst x = Left $ "Invalid for: " ++ show x

sExpForToAstArgs :: [SExpr] -> Either String ([Ast], Ast, [Ast])
sExpForToAstArgs [SLine [], cond, SLine []] = sExpInstructionToAst [cond] >>= \condAst -> Right ([], condAst, [])
sExpForToAstArgs [SLine [init], cond, SLine []] = sExpInstructionToAst [init] >>= \initAst -> sExpInstructionToAst [cond] >>= \condAst -> Right ([initAst], condAst, [])
sExpForToAstArgs [SLine [], cond, SLine [inc]] = sExpInstructionToAst [cond] >>= \condAst -> sExpInstructionToAst [inc] >>= \incAst -> Right ([], condAst, [incAst])
sExpForToAstArgs [SLine [init], cond, SLine [inc]] = sExpInstructionToAst [init] >>= \initAst -> sExpInstructionToAst [cond] >>= \condAst -> sExpInstructionToAst [inc] >>= \incAst -> Right ([initAst], condAst, [incAst])
sExpForToAstArgs x = Left $ "Invalid for args: " ++ show x

sExpStructToAst :: [[SExpr]] -> Either String Ast
sExpStructToAst [] = Right $ AList []
sExpStructToAst (x:xs) = case sExpStructToAst' x of
  Right astElem -> case sExpStructToAst xs of
    Right (AList []) -> Right $ AObject [astElem]
    Right (AObject elems) -> Right $ AObject (astElem : elems)
    Left err -> Left err
  Left err -> Left err

sExpStructToAst' :: [SExpr] -> Either String AstObjectElement
sExpStructToAst' (SSymbol key: SSymbol ":" : value: _) = sExpInstructionToAst [value] >>= \valueAst -> Right $ ObjectElement {objectKey = key, objectValue = valueAst}
sExpStructToAst' x = Left $ "Invalid struct: " ++ show x

sExpInstructionToAst :: [SExpr] -> Either String Ast
sExpInstructionToAst (SSymbol "function": xs) = sExpFunctionToAst xs
sExpInstructionToAst (SSymbol "let": xs) = sExpVarAssignationToAst xs
sExpInstructionToAst (var: SSymbol "=" : value: xs) = sExpVarAssignationToAst [var, SSymbol "=", value]
sExpInstructionToAst (SSymbol x: _) | x `elem` ["break", "continue"] = Right $ ACall FuncCall {callFunction = ASymbol x, callArgs = []}
sExpInstructionToAst (SSymbol x: xs) | x `elem` ["!", "return", "print"] = sExpInstructionToAst xs >>= \ast -> Right $ ACall FuncCall {callFunction = ASymbol x, callArgs = [ast]}
sExpInstructionToAst (arg1: SSymbol x: arg2:_) | x `elem` ["+", "-", "*", "/", "%", "==", "<", ">", "<=", ">=", "&&", "||", "!="] = sExpBuilinFunctionToAst x [arg1, arg2]
sExpInstructionToAst (SSymbol "if": cond: body: _) = sExpIfToAst [cond, body]
sExpInstructionToAst (SSymbol "while": SParenthesis [cond]: SBracket [SLine body]:_) = sExpWhileToAst [cond, SLine body]
sExpInstructionToAst (SSymbol "for": xs) = sExpForToAst xs
sExpInstructionToAst (SSymbol x: SArray i: SSymbol "=" : value: xs) = sExpInstructionToAst [value] >>= \valueAst -> sExpInstructionToAst [SArray i] >>= \arrayAst -> Right $ AAssignation (VarAssignation { assignationKey = x, assignationValue = ACall ArrayAccess { accessArray = arrayAst, accessArg = valueAst}})
sExpInstructionToAst (SSymbol x: SArray i: xs) = sExpInstructionToAst i >>= \idx -> Right $ ACall ArrayAccess {accessArray = ASymbol x, accessArg = idx}
sExpInstructionToAst (SStruct x: _) = sExpStructToAst x
sExpInstructionToAst (SInt x:_) = Right $ AInt x
sExpInstructionToAst (SBool x:_) = Right $ ABool x
sExpInstructionToAst (SFloat x:_) = Right $ AFloat x
sExpInstructionToAst (SSymbol "undefined":_) = Right AUndefined
sExpInstructionToAst (SSymbol x:_) = Right $ ASymbol x
sExpInstructionToAst (SString x:_) = Right $ AString x
sExpInstructionToAst (SArray x:_) = AList <$> mapM sExpInstructionToAst [x]
sExpInstructionToAst (SParenthesis x:_) = AList <$> mapM sExpInstructionToAst [x]
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
