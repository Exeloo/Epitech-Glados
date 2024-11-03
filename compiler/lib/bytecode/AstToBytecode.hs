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
--handleCallFunction "+" args p = builtinAdd args p
--handleCallFunction "-" args p = builtinSub args p
--handleCallFunction "*" args p = builtinMul args p
--handleCallFunction "/" args p = builtinDiv args p
--handleCallFunction "%" args p = builtinMod args p
--handleCallFunction "!" args p = builtinNot args p
--handleCallFunction "&&" args p = builtinAnd args p
--handleCallFunction "||" args p = builtinOr args p
--handleCallFunction "==" args p = builtinEq args p
--handleCallFunction "!=" args p = builtinNotEq args p
--handleCallFunction "<" args p = builtinLess args p
--handleCallFunction ">" args p = builtinMore args p
--handleCallFunction "<=" args p = builtinLessEq args p
--handleCallFunction ">=" args p = builtinMoreEq args p
--handleCallFunction "if" args p = builtinIf args p
--handleCallFunction "return" args p = builtinReturn args p
--handleCallFunction "break" args p = builtinBreak args p
--handleCallFunction "continue" args p = builtinContinue args p
--handleCallFunction "print" args p = builtinPrint args p
handleCallFunction name a (path, args, vars, labels) = case getVar name (path, args, vars, labels) of
  Just var ->
    if isInFunction name path
    then handleRecursiveFunction var a (path, args, vars, labels)
    else handleFunctionDeclaration name var a (path, args, vars, labels)
  Nothing -> ((path, args, vars, labels), Left ("Invalid call: function \"" ++ name ++ "\" does not exist."))

handleRecursiveFunction :: Ast -> [Ast] -> BParams -> PBResult
handleRecursiveFunction (ADeclaration (FuncDeclaration { declareArgs = dArgs })) args p =
  handleRecursiveFunctionParams dArgs args p
handleRecursiveFunction value _ p = (p, Left ("Invalid call: " ++ show value ++ " is not callable."))

handleRecursiveFunctionParams :: [Symbol] -> [Ast] -> BParams -> PBResult
handleRecursiveFunctionParams [] [] p = (p2, r1)
  where
    (label, (p1, r1)) = goToFunctionInLabel p
    p2 = createScope label p1
handleRecursiveFunctionParams (sym:syms) (arg:args) p = (p4, (++) <$> ((++) <$> r1 <*> Right r2) <*> r3)
  where
      (p2, r1) = pushValueOnStack arg p
      (p3, r2) = addArgToScope sym p2
      (p4, r3) = handleRecursiveFunctionParams syms args p3
handleRecursiveFunctionParams _ _ p = (p, Left "Invalid call: Invalid number of argument")

handleFunctionDeclaration :: Symbol -> Ast -> [Ast] -> BParams -> PBResult
handleFunctionDeclaration name (ADeclaration (FuncDeclaration { declareArgs = dArgs, declareBody = dBody })) args p = handleFunctionLabels name dArgs args dBody p
handleFunctionDeclaration name value _ p = (p, Left ("Invalid call: " ++ show value ++ " is not callable."))

handleFunctionLabels :: Symbol -> [Symbol] -> [Ast] -> Ast -> BParams -> PBResult
handleFunctionLabels name argNames argValues body (path, args, vars, labels) =
  (p4, (++) <$> ((++) <$> ((++) <$> Right (getLabel inLabel) <*> r1) <*> Right (getLabel outLabel)) <*> Right r2)
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
handleForLoopLabels ass cond inc body (path, args, vars, labels) =
  handleForLoopAssignation ass cond inc body inLabel outLabel p
    where
      (label, labels2) = getWhileLabel labels
      p = createScope label (path, args, vars, labels2)
      inLabel = getLabelWithIO label 0
      outLabel = getLabelWithIO label 1

handleForLoopAssignation :: [Ast] -> Ast -> [Ast] -> Ast -> String -> String -> BParams -> PBResult
handleForLoopAssignation [] cond inc body inLabel outLabel p =
  (p1, (++) <$> Right (getLabel inLabel) <*> r1)
    where
      (p1, r1) = handleForLoopBody cond inc body inLabel outLabel p
handleForLoopAssignation ((AAssignation ass):_) cond inc body inLabel outLabel p =
  (p2, (++) <$> ((++) <$> r1 <*> Right (getLabel inLabel)) <*> r2)
    where
      (p1, r1) = parseAssignation ass p
      (p2, r2) = handleForLoopBody cond inc body inLabel outLabel p1
handleForLoopAssignation ass _ _ _ _ _ p = (p, Left ("Invalid for loop: Assignation must be an assignation but is " ++ show ass))

handleForLoopBody :: Ast -> [Ast] -> Ast -> String -> String -> BParams -> PBResult
handleForLoopBody cond inc body inLabel outLabel p = (p2, (++) <$> r1 <*> r2)
  where
    (p1, r1) = handleLoopCondition cond outLabel body p
    (p2, r2) = handleForLoopIncrementation inc inLabel outLabel p1

handleForLoopIncrementation :: [Ast] -> String -> String -> BParams -> PBResult
handleForLoopIncrementation [] inLabel outLabel p =
  (p1, (++) <$> ((++) <$> Right (getJump inLabel) <*> Right (getLabel outLabel)) <*> Right r1)
    where
      (p1, r1) = clearScope p
handleForLoopIncrementation ((AAssignation inc):_) inLabel outLabel p =
  (p2, (++) <$> ((++) <$> ((++) <$> r1 <*> Right (getJump inLabel)) <*> Right (getLabel outLabel)) <*> Right r2)
    where
      (p1, r1) = parseAssignation inc p
      (p2, r2) = clearScope p1
handleForLoopIncrementation inc _ _ p = (p, Left ("Invalid for loop: Incrementation must be an assignation but is " ++ show inc))

parseWhileLoop :: Ast -> Ast -> BParams -> PBResult
parseWhileLoop cond body p = handleWhileLoopLabels cond body p

handleWhileLoopLabels :: Ast -> Ast -> BParams -> PBResult
handleWhileLoopLabels cond body (path, args, vars, labels) = (p4, (++) <$> ((++) <$> ((++) <$> ((++) <$> Right (getLabel inLabel) <*> r1) <*> Right (getJump inLabel)) <*> Right (getLabel outLabel)) <*> Right r2)
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
  then parseOldAssignation key value params
  else parseNewAssignation key value params
parseAssignation (AccessAssignation { assignationAccessArray = (ASymbol arr), assignationAccessArg = arg, assignationAccessValue = value }) params = modifyArg arr (modifyArray (ASymbol arr) arg value) params
parseAssignation (AccessAssignation { assignationAccessArray = arr, assignationAccessArg = arg, assignationAccessValue = value }) params = (params, modifyArray arr arg value params)

parseNewAssignation :: String -> Ast -> BParams -> PBResult
parseNewAssignation key value params = (n2Params, (++) <$> r1 <*> Right r2)
  where
    (n1Params, r1) = pushValueOnStack value params
    (n2Params, r2) = addArgToScope key n1Params

parseOldAssignation :: String -> Ast -> BParams -> PBResult
parseOldAssignation key value params = modifyArg key (modifyVar value) params

modifyVar :: Ast -> BParams -> BResult
modifyVar value p = (++) <$> ((++) <$> Right getPopArg <*> r1) <*> Right getPushStackOnArg
  where
    (p1, r1) = pushValueOnStack value p

modifyArray :: Ast -> Ast -> Ast -> BParams -> BResult
modifyArray arr arg value params =
  (++) <$> ((++) <$> ((++) <$> r1 <*> r2) <*> r3) <*> Right syscallModifyArray
    where
      (p1, r1) = pushValueOnStack arr params
      (p2, r2) = pushValueOnStack arg p1
      (_, r3) = pushValueOnStack value p2
