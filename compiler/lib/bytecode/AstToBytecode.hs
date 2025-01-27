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
astToBytecode (ALine x) = (++) <$> r <*> Right getRet
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
handleCallFunction "+" args p = builtinAdd args p
handleCallFunction "-" args p = builtinSub args p
handleCallFunction "*" args p = builtinMul args p
handleCallFunction "/" args p = builtinDiv args p
handleCallFunction "%" args p = builtinMod args p
handleCallFunction "!" args p = builtinNot args p
handleCallFunction "&&" args p = builtinAnd args p
handleCallFunction "||" args p = builtinOr args p
handleCallFunction "==" args p = builtinEq args p
handleCallFunction "!=" args p = builtinNotEq args p
handleCallFunction "<" args p = builtinLess args p
handleCallFunction ">" args p = builtinMore args p
handleCallFunction "<=" args p = builtinLessEq args p
handleCallFunction ">=" args p = builtinMoreEq args p
handleCallFunction "if" args p = builtinIf args p
handleCallFunction "return" args p = builtinReturn args p
handleCallFunction "break" args p = builtinBreak args p
handleCallFunction "continue" args p = builtinContinue args p
handleCallFunction "print" args p = builtinPrint args p
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
handleFunctionDeclaration _ value _ p = (p, Left ("Invalid call: " ++ show value ++ " is not callable."))

handleFunctionLabels :: Symbol -> [Symbol] -> [Ast] -> Ast -> BParams -> PBResult
handleFunctionLabels name argNames argValues body (path, args, vars, labels) =
  (p2, (++) <$> ((++) <$> r1 <*> Right (getLabel outLabel)) <*> Right r2)
    where
      (label, labels2) = getFunctionLabel name labels
      p = createScope label (path, args, vars, labels2)
      inLabel = getLabelWithIO label 0
      outLabel = getLabelWithIO label 1
      (p1, r1) = handleFunctionParams (getLabel inLabel) argNames argValues body p
      (p2, r2) = clearScope p1

handleFunctionParams :: String -> [Symbol] -> [Ast] -> Ast -> BParams -> PBResult
handleFunctionParams label [] [] body p = (p1, (++) <$> Right (label) <*> r1)
  where
    (p1, r1) = handleFunctionBody body p
handleFunctionParams label (sym:syms) (arg:args) body p = (p3, (++) <$> ((++) <$> r1 <*> Right r2) <*> r3)
  where
      (p1, r1) = pushValueOnStack arg p
      (p2, r2) = addArgToScope sym p1
      (p3, r3) = handleFunctionParams label syms args body p2
handleFunctionParams _ _ _ _ p = (p, Left "Invalid call: Invalid number of argument")

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
      (label, labels2) = getForLabel labels
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
parseAssignation (AccessAssignation { assignationAccessArray = (ASymbol arr), assignationAccessArg = arg, assignationAccessValue = value }) params = modifyArray arr (ASymbol arr) arg value params
parseAssignation (AccessAssignation { assignationAccessArray = arr }) params = (params, Left ("Invalid access assignation: the array/object that is access must be instanciated in a variable, but is " ++ show arr))

parseNewAssignation :: String -> Ast -> BParams -> PBResult
parseNewAssignation key value params = (n2Params, (++) <$> r1 <*> Right r2)
  where
    (n1Params, r1) = pushValueOnStack value params
    (n2Params, r2) = addArgToScope key n1Params

parseOldAssignation :: String -> Ast -> BParams -> PBResult
parseOldAssignation key value params = modifyVar key value params

modifyVar :: String -> Ast -> BParams -> PBResult
modifyVar name value p = (p1, (++) <$> r1 <*> modifyArg name p)
  where
    (p1, r1) = pushValueOnStack value p

modifyArray :: String -> Ast -> Ast -> Ast -> BParams -> PBResult
modifyArray name arr arg value p =
  (p3, (++) <$> ((++) <$> ((++) <$> ((++) <$> r1 <*> r2) <*> r3) <*> Right syscallModifyArray) <*> modifyArg name p)
    where
      (p1, r1) = pushValueOnStack arr p
      (p2, r2) = pushValueOnStack arg p1
      (p3, r3) = pushValueOnStack value p2

-- Parse If

parseIf :: Ast -> Ast -> BParams -> PBResult
parseIf cond body p = handleIfLabels cond body p

handleIfLabels :: Ast -> Ast -> BParams -> PBResult
handleIfLabels cond body (path, args, vars, labels) =
  (p2, (++) <$> ((++) <$> r1 <*> Right (getLabel outLabel)) <*> Right r2)
  where
    (label, labels2) = getIfLabel labels
    p = createScope label (path, args, vars, labels2)
    outLabel = getLabelWithIO label 1
    (p1, r1) = handleIfCondition cond outLabel body p
    (p2, r2) = clearScope p1

handleIfCondition :: Ast -> String -> Ast -> BParams -> PBResult
handleIfCondition cond label body p = (p2, (++) <$> ((++) <$> r1 <*> Right (getJumpIfFalse label)) <*> r2)
  where
    (p1, r1) = pushValueOnStack cond p
    (p2, r2) = handleIfBody body p1

handleIfBody :: Ast -> BParams -> PBResult
handleIfBody (ALine body) p = parseBlock body p
handleIfBody e p = (p, Left ("Invalid if body: the body of a loop must be a block, but is " ++ show e))

-- Builtins

builtinAdd :: [Ast] -> BParams -> PBResult
builtinAdd [a, b] p = callSyscall "Add" [a, b] p
builtinAdd _ p = (p, Left "Invalid Builtin call: Add require 2 params")

builtinSub :: [Ast] -> BParams -> PBResult
builtinSub [a, b] p = callSyscall "Sub" [a, b] p
builtinSub _ p = (p, Left "Invalid Builtin call: Sub require 2 params")

builtinMul :: [Ast] -> BParams -> PBResult
builtinMul [a, b] p = callSyscall "Mul" [a, b] p
builtinMul _ p = (p, Left "Invalid Builtin call: Mul require 2 params")

builtinDiv :: [Ast] -> BParams -> PBResult
builtinDiv [a, b] p = callSyscall "Div" [a, b] p
builtinDiv _ p = (p, Left "Invalid Builtin call: Div require 2 params")

builtinMod :: [Ast] -> BParams -> PBResult
builtinMod [a, b] p = callSyscall "Mod" [a, b] p
builtinMod _ p = (p, Left "Invalid Builtin call: Mod require 2 params")

builtinNot :: [Ast] -> BParams -> PBResult
builtinNot [a] p = callSyscall "Not" [a] p
builtinNot _ p = (p, Left "Invalid Builtin call: Not require 1 params")

builtinAnd :: [Ast] -> BParams -> PBResult
builtinAnd [a, b] p = callSyscall "And" [a, b] p
builtinAnd _ p = (p, Left "Invalid Builtin call: And require 2 params")

builtinOr :: [Ast] -> BParams -> PBResult
builtinOr [a, b] p = callSyscall "Or" [a, b] p
builtinOr _ p = (p, Left "Invalid Builtin call: Or require 2 params")

builtinEq :: [Ast] -> BParams -> PBResult
builtinEq [a, b] p = callSyscall "Eq" [a, b] p
builtinEq _ p = (p, Left "Invalid Builtin call: Eq require 2 params")

builtinNotEq :: [Ast] -> BParams -> PBResult
builtinNotEq [a, b] p = parseLine (ACall (FuncCall { callFunction = (ASymbol "!"), callArgs = [(ACall (FuncCall { callFunction = (ASymbol "=="), callArgs = [a, b] }))] })) p
builtinNotEq _ p = (p, Left "Invalid Builtin call: NotEq require 2 params")

builtinLess :: [Ast] -> BParams -> PBResult
builtinLess [a, b] p = callSyscall "Less" [a, b] p
builtinLess _ p = (p, Left "Invalid Builtin call: Less require 2 params")

builtinMore :: [Ast] -> BParams -> PBResult
builtinMore [a, b] p = parseLine (ACall (FuncCall { callFunction = (ASymbol "!"), callArgs = [(ACall (FuncCall { callFunction = (ASymbol "<="), callArgs = [a, b] }))] })) p
builtinMore _ p = (p, Left "Invalid Builtin call: More require 2 params")

builtinLessEq :: [Ast] -> BParams -> PBResult
builtinLessEq [a, b] p = parseLine (ACall (FuncCall { callFunction = (ASymbol "||"), callArgs = [(ACall (FuncCall { callFunction = (ASymbol "<"), callArgs = [a, b] })), (ACall (FuncCall { callFunction = (ASymbol "=="), callArgs = [a, b] }))] })) p
builtinLessEq _ p = (p, Left "Invalid Builtin call: LessEq require 2 params")

builtinMoreEq :: [Ast] -> BParams -> PBResult
builtinMoreEq [a, b] p = parseLine (ACall (FuncCall { callFunction = (ASymbol "!"), callArgs = [(ACall (FuncCall { callFunction = (ASymbol "<"), callArgs = [a, b] }))] })) p
builtinMoreEq _ p = (p, Left "Invalid Builtin call: MoreEq require 2 params")

builtinIf :: [Ast] -> BParams -> PBResult
builtinIf [a, b] p = parseIf a b p
builtinIf _ p = (p, Left "Invalid Builtin call: If require 2 params")

builtinReturn :: [Ast] -> BParams -> PBResult
builtinReturn [] p = builtinReturn [AUndefined] p
builtinReturn [a] p = (p2, (++) <$> r1 <*> r2)
  where
    (p1, r1) = pushValueOnStack a p
    (p2, r2) = callReturnBuiltin p1
builtinReturn _ p = (p, Left "Invalid Builtin call: Return require 0 or 1 params")

builtinBreak :: [Ast] -> BParams -> PBResult
builtinBreak [] p = goToLoopOutLabel p
builtinBreak _ p = (p, Left "Invalid Builtin call: Break require no params")

builtinContinue :: [Ast] -> BParams -> PBResult
builtinContinue [] p = goToLoopInLabel p
builtinContinue _ p = (p, Left "Invalid Builtin call: Continue require no params")

builtinPrint :: [Ast] -> BParams -> PBResult
builtinPrint [a] p = callSyscall "Print" [a] p
builtinPrint _ p = (p, Left "Invalid Builtin call: Print require 1 params")

callSyscall :: String -> [Ast] -> BParams -> PBResult
callSyscall sys args p = (p1, (++) <$> r1 <*> Right ((getPush sys) ++ getCall))
  where
    (p1, r1) = pushArgsOnStack args p

pushArgsOnStack :: [Ast] -> BParams -> PBResult
pushArgsOnStack [] p = (p, Right "")
pushArgsOnStack (x:xs) p = (p2, (++) <$> r2 <*> r1)
  where
    (p1, r1) = pushValueOnStack x p
    (p2, r2) = pushArgsOnStack xs p1

callReturnBuiltin :: BParams -> PBResult
callReturnBuiltin (path, args, vars, labels) =
  if (findFunctionLabel path) == Nothing
  then ((path, args, vars, labels), Right getRet)
  else goToFunctionOutLabel (path, args, vars, labels)
