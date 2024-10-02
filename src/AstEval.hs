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
callAST _ _ = Left "Invalid function"

replaceSymbol :: Symbol -> Ast -> Ast -> Ast
replaceSymbol s (a) (ASymbol b) = case s == b of
  True -> a
  False -> ASymbol b
replaceSymbol s (a) (ACall FuncCall {callFunction = f, callArgs = args}) = case map (replaceSymbol s (a)) args of
  [ASymbol s'] -> ASymbol s'
  args' -> ACall FuncCall {callFunction = f, callArgs = args'}
replaceSymbol s (a) (AList lst) = case map (replaceSymbol s (a)) lst of
  [ASymbol s'] -> ASymbol s'
  lst' -> AList lst'
replaceSymbol _ _ b = b

evalAstFunc :: [Symbol] -> Ast -> [Ast] -> Either String Ast
evalAstFunc (arg:args) func (argcall:argcalls) = evalAstFunc args (replaceSymbol arg argcall func) argcalls
evalAstFunc _ (ACall FuncCall {callFunction = FSymbol f, callArgs = args}) _ | f `elem` ["+", "-", "*", "div", "mod", "eq?", "<", "if"] = callAST f args
evalAstFunc _ (ACall FuncCall {callFunction = FFunc f, callArgs = args}) _ = evalAST (ACall FuncCall {callFunction = FFunc f, callArgs = [AInt 1]})
evalAstFunc _ (AList [body]) [] = evalAstFunc [] body []
evalAstFunc [] func [] = Right func
evalAstFunc _ _ [] = Left "Invalid function, not enough arguments"
evalAstFunc [] _ _ = Left "Invalid function, too many arguments"

evalAST :: Ast-> Either String Ast
evalAST (AInt x) = Right (AInt x)
evalAST (ASymbol x) = Right (ASymbol x)
evalAST (ACall FuncCall {callFunction = FSymbol func, callArgs = args})= case mapM evalAST args of
    Right x -> case callAST func x of
        Right x -> Right x
        Left x -> Left x
    Left x -> Left x
evalAST (ACall FuncCall {callFunction = FFunc FuncDeclaration { declareArgs = argfunc, declareBody = body }, callArgs = args}) = case evalAstFunc argfunc (AList body) args of
  Right x -> Right x
  Left x -> Left x
evalAST (AList x) = case mapM evalAST x of
    Right x -> Right (AList x)
    Left x -> Left x
