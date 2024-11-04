{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- SExpToAst
-}

module SExprToAst (sExpFunctionToAst, sExpVarAssignationToAst, sExpBuilinFunctionToAst, sExpIfToAst, sExpWhileToAst, sExpForToAst, sExpStructToAst, sExpInstructionToAst, sExpToAst) where

import AstData
import SExprData

sExpFunctionToAst :: [SExpr] -> Either String Ast
sExpFunctionToAst (SSymbol name : SParenthesis args : SBracket body : _) = mapM sExpFunctionToAstSymbol args >>= \argSymbols -> case sExpInstructionToAst body of
       Right (ALine bodyAst) -> Right $ AAssignation $ VarAssignation
         { assignationKey = name
         , assignationValue = ADeclaration $ FuncDeclaration
           { declareArgs = argSymbols
           , declareBody = (ALine bodyAst)
           }
         }
       Right bodyAst -> Right $ AAssignation $ VarAssignation
          { assignationKey = name
          , assignationValue = ADeclaration $ FuncDeclaration
            { declareArgs = argSymbols
            , declareBody = (ALine [bodyAst])
            }
          }
       Left err -> Left err
sExpFunctionToAst _ = Left "Invalid function"

sExpFunctionToAstSymbol :: SExpr -> Either String String
sExpFunctionToAstSymbol (SLine [SSymbol x]) = Right x
sExpFunctionToAstSymbol x = Left $ "Invalid function symbol: " ++ show x

sExpVarAssignationToAst :: [SExpr] -> Either String Ast
sExpVarAssignationToAst (SSymbol name: SSymbol "=" : value) = case sExpInstructionToAst value of
  Right valueAst -> Right $ AAssignation (VarAssignation {
      assignationKey = name,
      assignationValue = valueAst
    })
  Left err -> Left err
sExpVarAssignationToAst x = Left $ "Invalid assignation: " ++ show x

sExpBuilinFunctionToAst :: String -> [SExpr] -> Either String Ast
sExpBuilinFunctionToAst opp [arg1, arg2] = sExpInstructionToAst [arg1] >>= \arg1Ast -> sExpInstructionToAst [arg2] >>= \arg2Ast -> Right $ ACall FuncCall {
  callFunction = ASymbol opp,
  callArgs = [arg1Ast, arg2Ast]
}
sExpBuilinFunctionToAst a b = Left $ "Invalid builtin function: " ++ a ++ " with args: " ++ show b

sExpIfToAst :: [SExpr] -> Either String Ast
sExpIfToAst (cond: body: _) = case sExpInstructionToAst [cond] of
  Right condAst -> case sExpInstructionToAst [body] of
    Right (ALine bodyAst) -> Right $ ACall FuncCall {
          callFunction = ASymbol "if",
          callArgs = [condAst, (ALine bodyAst)]
        }
    Right bodyAst -> Right $ ACall FuncCall {
      callFunction = ASymbol "if",
      callArgs = [condAst, (ALine [bodyAst])]
    }
    Left err -> Left err
  Left err -> Left err
sExpIfToAst x = Left $ "Invalid if: " ++ show x

sExpWhileToAst :: [SExpr] -> Either String Ast
sExpWhileToAst (cond: body: _) = case sExpInstructionToAst [cond] of
  Right condAst -> case sExpInstructionToAst [body] of
    Right (ALine bodyAst) -> Right $ ALoop $ WhileLoop {
      whileCondition = condAst,
      whileBody = (ALine bodyAst)
    }
    Right bodyAst -> Right $ ALoop $ WhileLoop {
      whileCondition = condAst,
      whileBody = (ALine [bodyAst])
    }
    Left err -> Left err
  Left err -> Left err
sExpWhileToAst x = Left $ "Invalid while: " ++ show x

sExpForToAst :: [SExpr] -> Either String Ast
sExpForToAst (SParenthesis args: SBracket [SLine body]:_) = sExpForToAstArgs args >>= \(int, cond, inc) -> case sExpInstructionToAst [SLine body] of
  Right (ALine bodyAst) -> Right $ ALoop $ ForLoop {
    forAssignation = int,
    forCondition = cond,
    forIncrementation = inc,
    forBody = (ALine bodyAst)
  }
  Right bodyAst -> Right $ ALoop $ ForLoop {
      forAssignation = int,
      forCondition = cond,
      forIncrementation = inc,
      forBody = (ALine [bodyAst])
    }
  Left err -> Left err
sExpForToAst x = Left $ "Invalid for: " ++ show x

sExpForToAstArgs :: [SExpr] -> Either String ([Ast], Ast, [Ast])
sExpForToAstArgs [SLine [], cond, SLine []] = sExpInstructionToAst [cond] >>= \condAst -> Right ([], condAst, [])
sExpForToAstArgs [SLine int, cond, SLine []] = sExpInstructionToAst int >>= \initAst -> sExpInstructionToAst [cond] >>= \condAst -> Right ([initAst], condAst, [])
sExpForToAstArgs [SLine [], cond, SLine inc] = sExpInstructionToAst [cond] >>= \condAst -> sExpInstructionToAst inc >>= \incAst -> Right ([], condAst, [incAst])
sExpForToAstArgs [SLine int, cond, SLine inc] = sExpInstructionToAst int >>= \initAst -> sExpInstructionToAst [cond] >>= \condAst -> sExpInstructionToAst inc >>= \incAst -> Right ([initAst], condAst, [incAst])
sExpForToAstArgs x = Left $ "Invalid for args: " ++ show x

sExpStructToAst :: [[SExpr]] -> Either String Ast
sExpStructToAst [] = Right $ AList []
sExpStructToAst (x:xs) = case sExpStructToAst' x of
  Right astElem -> case sExpStructToAst xs of
    Right (AList []) -> Right $ AObject [astElem]
    Right (AObject elems) -> Right $ AObject (astElem : elems)
    Right y -> Left $ "Invalid struct: " ++ show y
    Left err -> Left err
  Left err -> Left err

sExpStructToAst' :: [SExpr] -> Either String AstObjectElement
sExpStructToAst' (SSymbol key: SSymbol ":" : value: _) = sExpInstructionToAst [value] >>= \valueAst -> Right $ ObjectElement {objectKey = key, objectValue = valueAst}
sExpStructToAst' x = Left $ "Invalid struct: " ++ show x

sExpInstructionToAst :: [SExpr] -> Either String Ast
sExpInstructionToAst (SSymbol "function": xs) = sExpFunctionToAst xs
sExpInstructionToAst (SSymbol "let": xs) = sExpVarAssignationToAst xs
sExpInstructionToAst (var: SSymbol "=" : value) = sExpVarAssignationToAst [var, SSymbol "=", SLine value]
sExpInstructionToAst (SSymbol x: _) | x `elem` ["break", "continue"] = Right $ ACall FuncCall {callFunction = ASymbol x, callArgs = []}
sExpInstructionToAst (SSymbol x: xs) | x `elem` ["!", "return"] = sExpInstructionToAst xs >>= \ast -> Right $ ACall FuncCall {callFunction = ASymbol x, callArgs = [ast]}
sExpInstructionToAst (arg1: SSymbol x: arg2) | x `elem` ["+", "-", "*", "/", "%", "==", "<", ">", "<=", ">=", "&&", "||", "!="] = sExpBuilinFunctionToAst x [arg1, SLine arg2]
sExpInstructionToAst (SSymbol "if": cond: body: _) = sExpIfToAst [cond, body]
sExpInstructionToAst (SSymbol "while": SParenthesis [cond]: SBracket [SLine body]:_) = sExpWhileToAst [cond, SLine body]
sExpInstructionToAst (SSymbol "for": xs) = sExpForToAst xs
sExpInstructionToAst (SSymbol x: SArray i: SSymbol "=" : value) = sExpInstructionToAst value >>= \valueAst -> sExpInstructionToAst [SArray i] >>= \arrayAst -> Right $ AAssignation (VarAssignation { assignationKey = x, assignationValue = ACall ArrayAccess { accessArray = arrayAst, accessArg = valueAst}})
sExpInstructionToAst (SSymbol x: SArray i: _) = sExpInstructionToAst i >>= \idx -> Right $ ACall ArrayAccess {accessArray = ASymbol x, accessArg = idx}
sExpInstructionToAst (SStruct x: _) = sExpStructToAst x
sExpInstructionToAst (SSymbol call: SParenthesis args:_) = mapM sExpInstructionToAst [args] >>= \argsAst -> Right $ ACall FuncCall {callFunction = ASymbol call, callArgs = argsAst}
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
