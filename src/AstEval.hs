{-
-- EPITECH PROJECT, 2024
-- bootstrap_haskell
-- File description:
-- AstEval
-}

module AstEval (callAST) where

import AstData

callAST :: String -> Ast -> Maybe Ast
callAST "+" (Lst [Inte x, Inte y]) = Just (Inte (x + y))
callAST "-" (Lst [Inte x, Inte y]) = Just (Inte (x - y))
callAST "*" (Lst [Inte x, Inte y]) = Just (Inte (x * y))
callAST "/" (Lst [Inte x, Inte y]) = Just (Inte (x `div` y))
callAST a b = Just (Lst [Sym a, b])

replaceSymbol :: Ast -> Ast -> Ast -> Ast
replaceSymbol (Sym s) (a) (Sym b) = case s == b of
  True -> a
  False -> Sym b
replaceSymbol (Sym s) (a) (Lst lst) = case map (replaceSymbol (Sym s) (a)) lst of
  [Sym s'] -> Sym s'
  lst' -> Lst lst'
replaceSymbol (Sym s) (a) (ACall FuncCall {callFunction = f, callArgs = Lst args}) = case map (replaceSymbol (Sym s) (a)) args of
  [Sym s'] -> Sym s'
  args' -> ACall FuncCall {callFunction = f, callArgs = Lst args'}
replaceSymbol (Sym s) (a) b = b

evalAstFunc :: [Ast] -> Ast -> [Ast] -> Maybe Ast
evalAstFunc (arg:args) func (argcall:argcalls) = evalAstFunc args (replaceSymbol arg argcall func) argcalls
evalAstFunc _ (ACall FuncCall {callFunction = f, callArgs = Lst args}) _ | f `elem` ["+", "-", "*", "/"] = callAST f (Lst args)
evalAstFunc [] func [] = Just func
evalAstFunc _ _ _ = Nothing
