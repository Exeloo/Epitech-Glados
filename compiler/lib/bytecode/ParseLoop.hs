{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- ParseLoop
-}

module ParseLoop (parseLoop) where

import AstData
import BytcodeOperations
import BytecodeTypes

import LabelUtils
import ScopeUtils
import StackUtils (pushValueOnStack)

import AstToBytecode (parseBlock)

parseLoop :: AstLoop -> BytecodeParams -> PBResult
parseLoop (ForLoop { forAssignation = ass, forCondition = cond, forIncrementation = inc, forBody = body }) p = parseForLoop ass cond inc body p
parseLoop (WhileLoop { whileCondition = cond, whileBody = body }) p = parseWhileLoop cond body p

parseForLoop :: Ast -> Ast -> Ast -> Ast -> BytecodeParams -> PBResult
parseForLoop ass cond inc body p = (++)

handleForLoopAssignation :: Ast -> Ast -> Ast -> Ast -> BytecodeParams -> PBResult
handleForLoopAssignation (AAssignation ass) cond inc body p

parseWhileLoop :: Ast -> Ast -> BytecodeParams -> PBResult
parseWhileLoop cond body p = handleWhileLoopLabels cond body p

handleWhileLoopLabels :: Ast -> Ast -> BytecodeParams -> PBResult
handleWhileLoopLabels cond body p = (p4, (++) <$> (getLabel inLabel) <*> r1 <*> outLabel <*> r2)
  where
    (label, p2) = getWhileLabel (createScope p)
    inLabel = getLabelWithIO label 0
    outLabel = getLabelWithIO label 1
    (p3, r1) = handleLoopCondition cond outLabel body p2
    (p4, r2) = clearScope p3

handleLoopCondition :: Ast -> String -> Ast -> BytecodeParams -> PBResult
handleLoopCondition cond label body p = (p3, (++) <$> r1 <*> (getJumpIfFalse label) <*> r2)
  where
    (p2, r1) = pushValueOnStack cond p
    (p3, r2) = handleLoopBody body p2

handleLoopBody :: Ast -> BytecodeParams -> PBResult
handleLoopBody (ALine body) p = parseBlock body p
handleLoopBody e p = (p, Left "Invalid body: the body of a loop must be a block, but is " ++ show e)
