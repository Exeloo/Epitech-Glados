{-
-- EPITECH PROJECT, 2024
-- bootstrap_haskell
-- File description:
-- AstEval
-}

module AstEval (evalAST, callAST, replaceSymbol) where

import Symbol
import AstData
import Evaluation

checkListSymbol :: [Ast] -> [[Ast]] -> Either String Bool
checkListSymbol [] _ = Right False
checkListSymbol (x:xs) a = case x of
  ASymbol s -> case checkElemList s a of
    Right True -> checkListSymbol xs a
    Right False -> Left ("Symbol " ++ s ++ " not found")
    Left x -> Left x
  _ -> checkListSymbol xs a

callAST :: Bool -> String -> [Ast] -> [[Ast]] -> Either String Ast
callAST False f args a = case checkListSymbol args a of
  Right _ -> callAST True f args a
  Left x -> Left x
callAST True "+" [AInt x, AInt y] _ = Right (AInt (x + y))
callAST True "-" [AInt x, AInt y] _ = Right (AInt (x - y))
callAST True "*" [AInt x, AInt y] _ = Right (AInt (x * y))
callAST True "div" [AInt x, AInt y] _ = Right (AInt (x `div` y))
callAST True "mod" [AInt x, AInt y] _ = Right (AInt (x `mod` y))
callAST True "eq?" [x, y] _ = Right (ABool (x == y))
callAST True "<" [AInt x, AInt y] _ = Right (ABool (x < y))
callAST True "if" [func, a, b] _ = case func of
  ABool True -> Right a
  ABool False -> Right b
  ASymbol "#t" -> Right a
  ASymbol "#f" -> Right b
  ACall FuncCall { callFunction = FSymbol f, callArgs = arg } -> case callAST True f arg [[]] of
    Right (ABool True) -> Right a
    Right (ABool False) -> Right b
  _ -> Left "Invalid if statement"
callAST _ _ _ _ = Left "Invalid function"

replaceSymbol :: Symbol -> Ast -> [[Ast]] -> Ast -> Ast
replaceSymbol s (a) var (ASymbol b)  | checkElemList b var == Right True = case getElemList b var of
  Right x -> x
  Left _ -> ASymbol b
replaceSymbol s (a) var (ASymbol b) | s == b = a
replaceSymbol s (a) var (ACall FuncCall {callFunction = f, callArgs = args}) = case map (replaceSymbol s (a) var) args  of
  [ASymbol s'] -> ASymbol s'
  args' -> ACall FuncCall {callFunction = f, callArgs = args'}
replaceSymbol s (a) var (AList lst) = case map (replaceSymbol s (a) var) lst  of
  [ASymbol s'] -> ASymbol s'
  lst' -> AList lst'
replaceSymbol _ _ _ b = b

evalAstFunc :: [Symbol] -> Ast -> [Ast] -> [[Ast]] -> Either String Ast
evalAstFunc (arg:args) func (argcall:argcalls) a = evalAstFunc args (replaceSymbol arg argcall a func) argcalls a
evalAstFunc _ (ACall FuncCall {callFunction = FSymbol f, callArgs = args}) _ a | f `elem` ["+", "-", "*", "div", "mod", "eq?", "<", "if"] = callAST False f args a
evalAstFunc _ (ACall FuncCall {callFunction = FFunc f, callArgs = args}) _ a = evalAST a (ACall FuncCall {callFunction = FFunc f, callArgs = [AInt 1]})
evalAstFunc _ (AList [body]) [] a = evalAstFunc [] body [] a
evalAstFunc [] func [] _= Right func
evalAstFunc _ _ [] _ = Left "Invalid function, not enough arguments"
evalAstFunc [] _ _ _ = Left "Invalid function, too many arguments"

evalAST :: [[Ast]] -> Ast -> Either String Ast
evalAST a (AInt x) = Right (AInt x)
evalAST a (ASymbol x) = Right (ASymbol x)
evalAST a (AAssignation var) = Right (AAssignation var)
evalAST a (ACall FuncCall {callFunction = FSymbol func, callArgs = args})= case mapM (evalAST a) args of
    Right x -> case callAST False func x a of
        Right x -> Right x
        Left x -> Left x
    Left x -> Left x
evalAST a (ACall FuncCall {callFunction = FFunc FuncDeclaration { declareArgs = argfunc, declareBody = body }, callArgs = args}) = case evalAstFunc argfunc (AList body) args a of
  Right x -> Right x
  Left x -> Left x
evalAST a (AList (x:xs)) = case evalAST a x of
  Right (AAssignation var) -> Right (addAssignation (AAssignation var) a) >>= \x -> evalAST x (AList xs)
  Right x -> evalAST a (AList xs)
  Left x -> Left x
