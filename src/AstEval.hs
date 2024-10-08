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
callAST "+" [x, y] = Right (AInt (evalArgInt x + evalArgInt y))
callAST "-" [x, y] = Right (AInt (evalArgInt x - evalArgInt y))
callAST "*" [x, y] = Right (AInt (evalArgInt x * evalArgInt y))
callAST "div" [x, y] = Right (AInt (evalArgInt x `div` evalArgInt y))
callAST "mod" [x, y] = Right (AInt (evalArgInt x `mod` evalArgInt y))
callAST "eq?" [x, y] = Right (ABool (x == y))
callAST "<" [x, y] = Right (ABool (evalArgInt x < evalArgInt y))
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
callAST a _ = Left $ "callAST : Invalid function: " ++ a

evalArgInt :: Ast -> Int
evalArgInt ast = case evalAST [[]] ast of
  Right (AInt x) -> x
  _ -> 0

replaceSymbol :: Symbol -> Ast -> [[Ast]] -> Ast -> Ast
replaceSymbol _ _ _ (ASymbol b)  | checkElemList b [[]] == Right True = case getElemList b [[]] of
  Right (AAssignation (VarAssignation {assignationKey = _, assignationValue = x})) -> x
  _ -> ASymbol b
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
evalAstFunc _ (AList [body]) [] a = evalAstFunc [] body [] a
evalAstFunc [] func [] _= Right (func)
evalAstFunc _ _ [] _ = Left "Invalid function, not enough arguments"
evalAstFunc [] _ _ _ = Left "Invalid function, too many arguments"

findFunc :: Symbol -> [Ast] -> [[Ast]] -> Either String Ast
findFunc func args _ = case getElemList func [[]] of
  Right (AAssignation (VarAssignation {assignationKey = _, assignationValue = ADeclaration func'})) -> Right (ACall FuncCall {callFunction = FFunc func', callArgs = args})
  Right x -> Left $ "Function not found1: " ++ show x
  Left x -> Left $ "Function not found2: " ++ x

evalAST :: [[Ast]] -> Ast -> Either String Ast
evalAST _ (AInt x) = Right (AInt x)
evalAST a (ASymbol x) = Right (replaceSymbol x (ASymbol x) a (ASymbol x))
-- evalAST a (AAssignation var) = Right (addAssignation (AAssignation var) a) >>= \x -> Left $ show x
evalAST a (AAssignation var) = case addAssignation (AAssignation var) a of
  [x] -> Left $ show (head x)
  x -> Left $ show x
evalAST a (ACall (FuncCall {callFunction = FSymbol func, callArgs = args})) | func `elem` ["+", "-", "*", "div", "mod", "eq?", "<", "if"] = case mapM (evalAST a) args of
    Right x -> case callAST func x of
        Right x' -> Right x'
        Left x' -> Left x'
    Left x -> Left x
evalAST a (ACall (FuncCall {callFunction = FSymbol func, callArgs = args})) = case findFunc func args a of
  Right x -> evalAST a x
  Left x -> Left x
evalAST a (ACall FuncCall {callFunction = FFunc FuncDeclaration { declareArgs = argfunc, declareBody = body }, callArgs = args}) = case evalAstFunc argfunc (AList body) args a of
  Right x -> Right x
  Left x -> Left x
evalAST a (AList x) = case mapM (evalAST a) x of
  Right x' -> Right (AList x')
  Left x' -> Left x'
evalAST _ _ = Left "Invalid function"
