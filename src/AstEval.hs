{-
-- EPITECH PROJECT, 2024
-- bootstrap_haskell
-- File description:
-- AstEval
-}

module AstEval (evalAST, callAST, replaceSymbol) where

import AstData

callAST :: String -> Ast -> Either String Ast
callAST "+" (AList [AInt x, AInt y]) = Right (AInt (x + y))
callAST "-" (AList [AInt x, AInt y]) = Right (AInt (x - y))
callAST "*" (AList [AInt x, AInt y]) = Right (AInt (x * y))
callAST "div" (AList [AInt x, AInt y]) = Right (AInt (x `div` y))
callAST "mod" (AList [AInt x, AInt y]) = Right (AInt (x `mod` y))
callAST "eq?" (AList [x, y]) = Right (ABool (x == y))
callAST "<" (AList [AInt x, AInt y]) = Right (ABool (x < y))
callAST "if" (AList [func, a, b]) = case func of
  ABool True -> Right a
  ABool False -> Right b
  ASymbol "#t" -> Right a
  ASymbol "#f" -> Right b
  ACall FuncCall { callFunction = FSymbol f, callArgs = arg } -> case callAST f (AList arg) of
    Right (ABool True) -> Right a
    Right (ABool False) -> Right b
  _ -> Left "Invalid if statement"
callAST a b = Right (AList [ASymbol a, b])

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

evalAstFunc :: [Ast] -> Ast -> [Ast] -> Either String Ast
evalAstFunc (arg:args) func (argcall:argcalls) = evalAstFunc args (replaceSymbol arg argcall func) argcalls
evalAstFunc _ (ACall FuncCall {callFunction = FSymbol f, callArgs = args}) _ | f `elem` ["+", "-", "*", "div", "mod", "eq?", "<", "if"] = callAST f (AList args)
evalAstFunc [] func [] = Right func
evalAstFunc _ _ _ = Left "Invalid function"

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
