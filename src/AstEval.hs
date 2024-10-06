{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- AstEval
-}

module AstEval (evalAST, callAST, replaceSymbol) where

import Symbol
import AstData
import Evaluation

callAST :: String -> [Ast] -> Either String Ast
callAST "+" [AInt x, AInt y] = Right (AInt (x + y))
callAST "-" [AInt x, AInt y] = Right (AInt (x - y))
callAST "*" [AInt x, AInt y] = Right (AInt (x * y))
callAST "div" [AInt x, AInt y] = Right (AInt (x `div` y))
callAST "mod" [AInt x, AInt y] = Right (AInt (x `mod` y))
callAST "eq?" [x, y] = Right (ABool (x == y))
callAST "<" [AInt x, AInt y] = Right (ABool (x < y))
callAST "if" [func, a, b] = case func of
  ABool True -> Right a
  ABool False -> Right b
  ASymbol "#t" -> Right a
  ASymbol "#f" -> Right b
  ACall FuncCall { callFunction = FSymbol f, callArgs = arg } -> case callAST f arg of
    Right (ABool True) -> Right a
    Right (ABool False) -> Right b
    _ -> Left "Invalid if statement"
  _ -> Left "Invalid if statement"
callAST _ _ = Left "Invalid function"

replaceSymbol :: Symbol -> Ast -> [[Ast]] -> Ast -> Ast
replaceSymbol _ _ var (ASymbol b)  | checkElemList b var == Right True = case getElemList b var of
  Right (AAssignation (VarAssignation {assignationKey = _, assignationValue = x})) -> x
  Left _ -> ASymbol b
replaceSymbol s (a) _ (ASymbol b) | s == b = a
replaceSymbol s (a) var (ACall FuncCall {callFunction = f, callArgs = args}) = case map (replaceSymbol s (a) var) args  of
  [ASymbol s'] -> ASymbol s'
  args' -> ACall FuncCall {callFunction = f, callArgs = args'}
replaceSymbol s (a) var (AList lst) = case map (replaceSymbol s (a) var) lst  of
  [ASymbol s'] -> ASymbol s'
  lst' -> AList lst'
replaceSymbol _ _ _ b = b

evalAstFunc :: [Symbol] -> Ast -> [Ast] -> [[Ast]] -> Either String Ast
evalAstFunc (arg:args) func (argcall:argcalls) a = evalAstFunc args (replaceSymbol arg argcall a func) argcalls a
evalAstFunc _ (ACall FuncCall {callFunction = FSymbol f, callArgs = args}) _ _ | f `elem` ["+", "-", "*", "div", "mod", "eq?", "<", "if"] = callAST f args
evalAstFunc _ (ACall FuncCall {callFunction = FFunc f, callArgs = args}) _ a = evalAST a (ACall FuncCall {callFunction = FFunc f, callArgs = args})
evalAstFunc _ (AList [body]) [] a = evalAstFunc [] body [] a
evalAstFunc [] func [] _= Right func
evalAstFunc _ _ [] _ = Left "Invalid function, not enough arguments"
evalAstFunc [] _ _ _ = Left "Invalid function, too many arguments"

evalAST :: [[Ast]] -> Ast -> Either String Ast
evalAST _ (AInt x) = Right (AInt x)
evalAST a (ASymbol x) = Right (replaceSymbol x (ASymbol x) a (ASymbol x))
evalAST _ (AAssignation var) = Right (AAssignation var)
evalAST a (ACall FuncCall {callFunction = FSymbol func, callArgs = args})= case mapM (evalAST a) args of
    Right x -> case callAST func x of
        Right x' -> Right x'
        Left x' -> Left x'
    Left x -> Left x
evalAST a (ACall FuncCall {callFunction = FFunc FuncDeclaration { declareArgs = argfunc, declareBody = body }, callArgs = args}) = case evalAstFunc argfunc (AList body) args a of
  Right x -> Right x
  Left x -> Left x
evalAST a (AList (x:xs)) = case evalAST a x of
  Right (AAssignation var) -> Right (addAssignation (AAssignation var) a) >>= \x' -> evalAST x' (AList xs)
  Right _ -> evalAST a (AList xs)
  Left x' -> Left x'
evalAST _ _ = Left "Invalid function"
