{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- ParseCall
-}

module ParseCall (parseCall) where

import AstData
import BytcodeOperations
import BytecodeTypes
import BytecodeSyscall

import LabelUtils (getFunctionLabel, getLabelWithIO)
import StackUtils (pushValueOnStack)
import ScopeUtils (hasVarInScopes, getVar, createScope)

parseCall :: AstCall -> BytecodeParams -> PBResult
parseCall (ArrayAccess { accessArray = arr, accessArg = index }) p = (p3, (++) <$> r1 <*> r2 <*> syscallAccessArray)
  where
    (p2, r1) = pushArrayToStack arr p
    (p3, r2) = pushIndexOnStack index p2
parseCall (FuncCall { callFunction = (ASymbol name), callArgs = args }) p = handleCallFunction name args p
parseCall value p = (p, Left "Invalid call: trying to call " ++ show value)

handleCallFunction :: Symbol -> [Ast] -> BytecodeParams -> PBResult
handleCallFunction "+" (n1:n2:_) p =
handleCallFunction "-" (n1:n2:_) p =
handleCallFunction "*" (n1:n2:_) p =
handleCallFunction "/" (n1:n2:_) p =
handleCallFunction name args p =
  if hasVarInScopes name p
  then callFunction (getVar name p) args p
  else (p, Left "Invalid call: function \"" ++ name ++ "\" does not exist.")

callFunction :: Ast -> [Ast] -> BytecodeParams -> PBResult
callFunction (ADeclaration (FuncDeclaration { declareArgs = dArgs, declareBody = dBody })) args p =
callFunction value _ p = (p, Left "Invalid call: " ++ show value ++ " is not callable.")

handleFunctionLabels :: [Symbol] -> [Ast] -> Ast -> BytecodeParams -> PBResult
handleFunctionLabels names args body p = (p4, (++) <$> (getLabel inLabel) <*> r1 <*> outLabel <*> r2)
  where
    (label, p2) = getFunctionLabel (createScope p)
    inLabel = getLabelWithIO label 0
    outLabel = getLabelWithIO label 1
    (p3, r1) = handleFunctionParams names args body p2
    (p4, r2) = clearScope p3

handleFunctionParams :: [Symbol] -> [Ast] -> Ast -> BytecodeParams -> PBResult
handleFunctionParams [] [] body p = handleFunctionBody body p
handleFunctionParams (sym:syms) (arg:args) body p = (p4, (++) <$> r1 <*> r2 <*> r3)
  where
      (p2, r1) = pushValueOnStack arg p
      (p3, r2) = addArgToScope key p2
      (p4, r3) = handleFunctionParams syms args body p3
handleFunctionParams _ _ _ p = (p, Left "Invalid call: Invalid number of argument")

handleFunctionBody :: Ast -> BytecodeParams -> PBResult
handleFunctionBody (ALine body) p = parseBlock body p
handleFunctionBody body p = (p, Left "Invalid call: the body of a function must be a block, but is " ++ show body)