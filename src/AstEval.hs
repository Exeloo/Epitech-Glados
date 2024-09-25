{-
-- EPITECH PROJECT, 2024
-- bootstrap_haskell
-- File description:
-- AstEval
-}

module AstEval (evalAST, callAST, replaceSymbol) where

import AstData

callAST :: String -> Ast -> Maybe Ast
callAST "+" (AList [AInt x, AInt y]) = Just (AInt (x + y))
callAST "-" (AList [AInt x, AInt y]) = Just (AInt (x - y))
callAST "*" (AList [AInt x, AInt y]) = Just (AInt (x * y))
callAST "div" (AList [AInt x, AInt y]) = Just (AInt (x `div` y))
callAST "mod" (AList [AInt x, AInt y]) = Just (AInt (x `mod` y))
callAST "eq?" (AList [AInt x, AInt y]) = Just (ABool (x == y))
callAST "<" (AList [AInt x, AInt y]) = Just (ABool (x < y))
callAST "if" (AList [func, a, b]) = case func of
  ABool True -> Just a
  ABool False -> Just b
  ASymbol "#t" -> Just a
  ASymbol "#f" -> Just b
  ACall FuncCall { callFunction = FSymbol f, callArgs = arg } -> case callAST f (AList arg) of
    Just (ABool True) -> Just a
    Just (ABool False) -> Just b
  _ -> Nothing
callAST a b = Just (AList [ASymbol a, b])

replaceSymbol :: Ast -> Ast -> Ast -> Ast
replaceSymbol (ASymbol s) (a) (ASymbol b) = case s == b of
  True -> a
  False -> ASymbol b
replaceSymbol (ASymbol s) (a) (AList lst) = case map (replaceSymbol (ASymbol s) (a)) lst of
  [ASymbol s'] -> ASymbol s'
  lst' -> AList lst'
replaceSymbol (ASymbol s) (a) (ACall FuncCall {callFunction = f, callArgs = args}) = case map (replaceSymbol (ASymbol s) (a)) args of
  [ASymbol s'] -> ASymbol s'
  args' -> ACall FuncCall {callFunction = f, callArgs = args'}
replaceSymbol _ _ b = b

evalAstFunc :: [Ast] -> Ast -> [Ast] -> Maybe Ast
evalAstFunc (arg:args) func (argcall:argcalls) = evalAstFunc args (replaceSymbol arg argcall func) argcalls
evalAstFunc _ (ACall FuncCall {callFunction = FSymbol f, callArgs = args}) _ | f `elem` ["+", "-", "*", "div", "mod", "eq?", "<", "if"] = callAST f (AList args)
evalAstFunc [] func [] = Just func
evalAstFunc _ _ _ = Nothing

evalAST :: Ast-> Maybe Ast
evalAST (AInt x) = Just (AInt x)
evalAST (ASymbol x) = Just (ASymbol x)
evalAST (ACall FuncCall {callFunction = FSymbol func, callArgs = args})= case mapM evalAST args of
    Just x -> case callAST func (AList x) of
        Just x -> Just x
        Nothing -> Nothing
    Nothing -> Nothing
evalAST (AList x) = case mapM evalAST x of
    Just x -> Just (AList x)
    Nothing -> Nothing
