{-
-- EPITECH PROJECT, 2024
-- bootstrap_haskell
-- File description:
-- AstEval
-}

module AstEval (callAST) where

import AstData

callAST :: String -> Ast -> Maybe Ast
callAST "+" (AList [AInt x, AInt y]) = Just (AInt (x + y))
callAST "-" (AList [AInt x, AInt y]) = Just (AInt (x - y))
callAST "*" (AList [AInt x, AInt y]) = Just (AInt (x * y))
callAST "div" (AList [AInt x, AInt y]) = Just (AInt (x `div` y))
callAST "mod" (AList [AInt x, AInt y]) = Just (AInt (x `mod` y))
callAST "eq?" (AList [AInt x, AInt y]) = Just (ABool (x == y))
callAST "<" (AList [AString x, AString y]) = Just (ABool (x == y))
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
evalAstFunc _ (ACall FuncCall {callFunction = FSymbol f, callArgs = args}) _ | f `elem` ["+", "-", "*", "/"] = callAST f (AList args)
evalAstFunc [] func [] = Just func
evalAstFunc _ _ _ = Nothing
