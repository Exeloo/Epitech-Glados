{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- AstToBytecode
-}

module AstToBytecode (
  astToBytecode,
  parseBlock
) where

import AstData
import Symbol (Symbol)
import BytecodeOperations
import BytecodeSyscall
import BytecodeTypes

import LabelUtils
import ScopeUtils
import StackUtils

import ParseDeclaration


astToBytecode :: Ast -> BResult
astToBytecode (ALine x) = r
  where
    (_, r) = parseBlock x ([], ([[]], 0), [[]], [])
astToBytecode _ = Left "Invalid code: The ast must start with a line"

parseBlock :: [Ast] -> BParams -> PBResult
parseBlock [] p = (p, Right "")
parseBlock (x:xs) p = (rParams, (++) <$> line <*> r)
  where
    (tParams, line) = parseLine x p
    (rParams, r) = parseBlock xs tParams

parseLine :: Ast -> BParams -> PBResult
parseLine (AAssignation x) p = parseAssignation x p
parseLine (ACall (FuncCall { callFunction = func, callArgs = args })) p = parseCall (FuncCall { callFunction = func, callArgs = args }) p
parseLine (ALoop x) p = parseLoop x p
parseLine x p = (p, Left ("Invalid line: " ++ show x))

-- Parse Call

parseCall :: AstCall -> BParams -> PBResult
parseCall (ArrayAccess { accessArray = arr, accessArg = index }) p = (p3, (++) <$> ((++) <$> r1 <*> r2) <*> Right syscallAccessArray)
  where
    (p2, r1) = pushValueOnStack arr p
    (p3, r2) = pushValueOnStack index p2
parseCall (FuncCall { callFunction = (ASymbol name), callArgs = args }) p = handleCallFunction name args p
parseCall value p = (p, Left ("Invalid call: trying to call " ++ show value))

handleCallFunction :: Symbol -> [Ast] -> BParams -> PBResult
--handleCallFunction "+" (n1:n2:_) p =
--handleCallFunction "-" (n1:n2:_) p =
--handleCallFunction "*" (n1:n2:_) p =
--handleCallFunction "/" (n1:n2:_) p =
handleCallFunction name args p = case getVar name p of
  Just var -> handleFunctionDeclaration name var args p
  Nothing -> (p, Left ("Invalid call: function \"" ++ name ++ "\" does not exist."))

handleFunctionDeclaration :: Symbol -> Ast -> [Ast] -> BParams -> PBResult
handleFunctionDeclaration name (ADeclaration (FuncDeclaration { declareArgs = dArgs, declareBody = dBody })) args p = handleFunctionLabels name dArgs args dBody p
handleFunctionDeclaration name value _ p = (p, Left ("Invalid call: " ++ show value ++ " is not callable."))

handleFunctionLabels :: Symbol -> [Symbol] -> [Ast] -> Ast -> BParams -> PBResult
handleFunctionLabels name argNames argValues body (path, args, vars, labels) =
  (p4, (++) <$> ((++) <$> ((++) <$> Right (getLabel inLabel) <*> r1) <*> Right outLabel) <*> Right r2)
    where
      (label, labels2) = getFunctionLabel name labels
      p = createScope label (path, args, vars, labels2)
      inLabel = getLabelWithIO label 0
      outLabel = getLabelWithIO label 1
      (p3, r1) = handleFunctionParams argNames argValues body p
      (p4, r2) = clearScope p3

handleFunctionParams :: [Symbol] -> [Ast] -> Ast -> BParams -> PBResult
handleFunctionParams [] [] body p = handleFunctionBody body p
handleFunctionParams (sym:syms) (arg:args) body p = (p4, (++) <$> ((++) <$> r1 <*> Right r2) <*> r3)
  where
      (p2, r1) = pushValueOnStack arg p
      (p3, r2) = addArgToScope sym p2
      (p4, r3) = handleFunctionParams syms args body p3
handleFunctionParams _ _ _ p = (p, Left "Invalid call: Invalid number of argument")

handleFunctionBody :: Ast -> BParams -> PBResult
handleFunctionBody (ALine body) p = parseBlock body p
handleFunctionBody body p = (p, Left ("Invalid call: the body of a function must be a block, but is " ++ show body))

-- Stack Utils

pushValueOnStack :: Ast -> BParams -> PBResult
pushValueOnStack (ASymbol sym) p = (p, pushArgOnStack sym p)
pushValueOnStack (ACall call) p = parseCall call p
pushValueOnStack (ALine x) p = (p, Left (invalidValueOnStack (ALine x)))
pushValueOnStack (ADeclaration x) p = (p, Left (invalidValueOnStack (ADeclaration x)))
pushValueOnStack (AAssignation x) p = (p, Left (invalidValueOnStack (AAssignation x)))
pushValueOnStack (ALoop x) p = (p, Left (invalidValueOnStack (ALoop x)))
pushValueOnStack value p = (p, Right (getPush (show value)))

-- Parse Loop

parseLoop :: AstLoop -> BParams -> PBResult
parseLoop (ForLoop { forAssignation = ass, forCondition = cond, forIncrementation = inc, forBody = body }) p = parseForLoop ass cond inc body p
parseLoop (WhileLoop { whileCondition = cond, whileBody = body }) p = parseWhileLoop cond body p

parseForLoop :: [Ast] -> Ast -> [Ast] -> Ast -> BParams -> PBResult
parseForLoop ass cond inc body p = handleForLoopLabels ass cond inc body p

handleForLoopLabels :: [Ast] -> Ast -> [Ast] -> Ast -> BParams -> PBResult
handleForLoopLabels ((AAssignation ass):_) cond ((AAssignation inc):_) body (path, args, vars, labels) =
  (p4, (++) <$> ((++) <$> ((++) <$> ((++) <$> ((++) <$> r1 <*> Right (getLabel inLabel)) <*> r2) <*> r3) <*> Right outLabel) <*> Right r4)
    where
      (label, labels2) = getWhileLabel labels
      p = createScope label (path, args, vars, labels2)
      inLabel = getLabelWithIO label 0
      outLabel = getLabelWithIO label 1
      (p1, r1) = parseAssignation ass p
      (p2, r2) = handleLoopCondition cond outLabel body p1
      (p3, r3) = parseAssignation inc p2
      (p4, r4) = clearScope p3
handleForLoopLabels ass _ inc _ p = (p, Left ("Invalid for loop: Assignation and Incrementation must be assignations but is " ++ show ass ++ " and " ++ show inc))

parseWhileLoop :: Ast -> Ast -> BParams -> PBResult
parseWhileLoop cond body p = handleWhileLoopLabels cond body p

handleWhileLoopLabels :: Ast -> Ast -> BParams -> PBResult
handleWhileLoopLabels cond body (path, args, vars, labels) = (p4, (++) <$> ((++) <$> ((++) <$> Right (getLabel inLabel) <*> r1) <*> Right outLabel) <*> Right r2)
  where
    (label, labels2) = getWhileLabel labels
    p = createScope label (path, args, vars, labels2)
    inLabel = getLabelWithIO label 0
    outLabel = getLabelWithIO label 1
    (p3, r1) = handleLoopCondition cond outLabel body p
    (p4, r2) = clearScope p3

handleLoopCondition :: Ast -> String -> Ast -> BParams -> PBResult
handleLoopCondition cond label body p = (p3, (++) <$> ((++) <$> r1 <*> Right (getJumpIfFalse label)) <*> r2)
  where
    (p2, r1) = pushValueOnStack cond p
    (p3, r2) = handleLoopBody body p2

handleLoopBody :: Ast -> BParams -> PBResult
handleLoopBody (ALine body) p = parseBlock body p
handleLoopBody e p = (p, Left ("Invalid body: the body of a loop must be a block, but is " ++ show e))

-- Parse Assignation

parseAssignation :: AstAssignation -> BParams -> PBResult
parseAssignation (VarAssignation { assignationKey = key, assignationValue = (ADeclaration value) }) params = parseDeclaration value key params
parseAssignation (VarAssignation { assignationKey = key, assignationValue = value }) params =
  if hasArgInScopes key params
  then parseNewAssignation key value params
  else parseOldAssignation key value params

parseNewAssignation :: String -> Ast -> BParams -> PBResult
parseNewAssignation key value params = (n2Params, (++) <$> r1 <*> Right r2)
  where
    (n1Params, r1) = pushValueOnStack value params
    (n2Params, r2) = addArgToScope key n1Params

parseOldAssignation :: String -> Ast -> BParams -> PBResult
parseOldAssignation key value params = (n2Params, (++) <$> r1 <*> Right r2)
  where
    (n1Params, r1) = pushValueOnStack value params
    (n2Params, r2) = addArgToScope key n1Params
