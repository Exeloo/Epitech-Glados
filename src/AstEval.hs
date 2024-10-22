{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- AstEval
-}

module AstEval (evalAST, callAST, replaceSymbol, ifAstEvaluation ,getSymbolFromList) where

import Symbol
import AstData
import Evaluation
import SExprToAst (evaluateAst)

ifAstFunctionCall :: String -> [Ast] -> Ast -> Ast -> Either String Ast
ifAstFunctionCall symbol args a b = case callAST symbol args of
    Right (ABool True) -> Right a
    Right (ABool False) -> Right b
    _ -> Left "Invalid if statement"

ifAstEvaluation :: [Ast] -> Either String Ast
ifAstEvaluation [func, a, b] = case func of
  ABool True -> Right a
  ABool False -> Right b
  ASymbol "#t" -> Right a
  ASymbol "#f" -> Right b
  ACall FuncCall { callFunction = FSymbol f, callArgs = arg } -> ifAstFunctionCall f arg a b
  _ -> Left "Invalid if statement"

callAST :: String -> [Ast] -> Either String Ast
callAST "+" [x, y] = Right (AInt (evalArgInt x + evalArgInt y))
callAST "-" [x, y] = Right (AInt (evalArgInt x - evalArgInt y))
callAST "*" [x, y] = Right (AInt (evalArgInt x * evalArgInt y))
callAST "div" [x, y] = Right (AInt (evalArgInt x `div` evalArgInt y))
callAST "mod" [x, y] = Right (AInt (evalArgInt x `mod` evalArgInt y))
callAST "eq?" [x, y] = Right (ABool (x == y))
callAST "<" [x, y] = Right (ABool (evalArgInt x < evalArgInt y))
callAST "if" x = ifAstEvaluation x
callAST a _ = Left $ "callAST : Invalid function: " ++ a

evalArgInt :: Ast -> Int
evalArgInt ast = case evalAST [[]] ast of
  Right (AInt x) -> x
  _ -> 0

getSymbolFromList :: [[Ast]] -> Symbol -> Ast
getSymbolFromList list b = case getElemList b list of
  Right (AAssignation (VarAssignation {assignationKey = _, assignationValue = x})) -> x
  _ -> ASymbol b

getFunctionFromList :: Symbol -> Ast -> [[Ast]] -> [Ast] -> AstFuncArg -> Ast
getFunctionFromList s (a) list args f = case map (replaceSymbol s (a) list) args  of
  [ASymbol s'] -> ASymbol s'
  args' -> ACall FuncCall {callFunction = f, callArgs = args'}

getListFromList :: Symbol -> Ast -> [[Ast]] -> [Ast] -> Ast
getListFromList s (a) var list = case map (replaceSymbol s (a) var) list of
  [ASymbol s'] -> ASymbol s'
  lst' -> AList lst'

replaceSymbol :: Symbol -> Ast -> [[Ast]] -> Ast -> Ast
replaceSymbol s (a) var (ASymbol b) | checkElemList b var == Right True = getSymbolFromList var b
                                     | s == b = a
replaceSymbol s (a) var (ACall FuncCall {callFunction = f, callArgs = args}) = getFunctionFromList s a var args f
replaceSymbol s (a) var (AList lst) = getListFromList s (a) var lst
replaceSymbol _ _ _ ast = ast

evalAstFunc :: [Symbol] -> Ast -> [Ast] -> [[Ast]] -> Either String Ast
evalAstFunc (arg:args) func (argcall:argcalls) a = evalAstFunc args (replaceSymbol arg argcall a func) argcalls a
evalAstFunc _ (ACall FuncCall {callFunction = FSymbol f, callArgs = args}) _ _ | f `elem` ["+", "-", "*", "div", "mod", "eq?", "<", "if"] = callAST f args
evalAstFunc _ (AList [body]) [] a = evalAstFunc [] body [] a
evalAstFunc [] func [] _= Right (func)
evalAstFunc _ _ [] _ = Left "Invalid function, not enough arguments"
evalAstFunc [] _ _ _ = Left "Invalid function, too many arguments"

findFunc :: Symbol -> [Ast] -> [[Ast]] -> Either String Ast
findFunc func args var = case getElemList func var of
  Right (AAssignation (VarAssignation {assignationKey = _, assignationValue = ADeclaration func'})) -> Right (ACall FuncCall {callFunction = FFunc func', callArgs = args})
  Right x -> Left $ "Function not found1: " ++ show x
  Left x -> Left $ "Function not found2: " ++ x

evalAssignation :: [[Ast]] -> Ast -> Either String Ast
evalAssignation a (AAssignation var) = case addAssignation (AAssignation var) a of
  [x] -> Right $ AString (show (head x))
  x -> Right $ AString (show x)
evalAssignation a b = evalAST a b

evalSyscall :: [[Ast]] -> Symbol ->[Ast] -> Either String Ast
evalSyscall a func args = case mapM (evalAST a) args of
    Right x -> callAST func x
    Left x -> Left x

evalFunction :: [[Ast]] -> Symbol -> [Ast] -> Either String Ast
evalFunction a func args = case findFunc func args a of
  Right x -> evalAST a x
  Left x -> Left x

evalFuncDeclaration :: [[Ast]] -> [Symbol] -> [Ast] -> [Ast] -> Either String Ast
evalFuncDeclaration  a argfunc body args = case evalAstFunc argfunc (AList body) args a of
  Right x -> Right x
  Left x -> Left x

evalList :: [[Ast]] -> [Ast] -> Either String Ast
evalList a x = case mapM (evalAST a) x of
  Right x' -> Right (AList x')
  Left x' -> Left x'

evalAST :: [[Ast]] -> Ast -> Either String [[Ast]]
evalAST a (AInt x) = evalResult (AInt x) a
evalAST a (ASymbol x) = evalResult (replaceSymbol x (ASymbol x) a (ASymbol x)) a
evalAST a (AAssignation var) = evalAssignation a (AAssignation var)
evalAST a (ACall (FuncCall {callFunction = FSymbol func, callArgs = args})) | syscall = evalResult (evalSyscall a func args) a
            where syscall = func `elem` ["+", "-", "*", "div", "mod", "eq?", "<", "if"]
evalAST a (ACall (FuncCall {callFunction = FSymbol func, callArgs = args})) = evalResult (evalFunction a func args) a
evalAST a (ACall FuncCall {callFunction = FFunc FuncDeclaration { declareArgs = argfunc, declareBody = body }, callArgs = args}) = evalResult (evalFuncDeclaration a argfunc body args) a
evalAST a (AList x) = evalResult (evalList a x) a
evalAST _ _ = Left "Invalid function"

evalResult :: Ast -> [[Ast]] -> Either String [[Ast]]
evalResult (ACall (FuncCall x y)) b = evaluateAst (ACall (FuncCall x y)) b
evalResult a b = show a >> Right b
evalResult _ _ = Left "Invalid function"
