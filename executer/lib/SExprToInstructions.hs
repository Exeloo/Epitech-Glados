module SExprToInstructions(sExprToInsts) where

import SExprData
import InstructionData

sValToValueData :: SValue -> ValueData
sValToValueData (SInt val) = VInt val
sValToValueData (SBool val) = VBool val
sValToValueData (SDouble val) = VDouble val
sValToValueData (SString val) = VString val
sValToValueData (SArray val) = VArray (map sValToValueData val)
sValToValueData (SValueCall SAdd) = VCall Add
sValToValueData (SValueCall SSub) = VCall Sub
sValToValueData (SValueCall SMul) = VCall Mul
sValToValueData (SValueCall SDiv) = VCall Div
sValToValueData (SValueCall SEq) = VCall Eq
sValToValueData (SValueCall SLess) = VCall Less


getLabelIndexInInst :: SExpr -> String -> Maybe Int
getLabelIndexInInst [] _ = Nothing
getLabelIndexInInst (SInstruction _ : s) name = 
    fmap (+1) (getLabelIndexInInst s name)
getLabelIndexInInst (SLabel val : s) name
    | val == name = Just 0
    | otherwise = getLabelIndexInInst s name

sInstToInst :: SExpr -> SInst -> Either String InstructionData
sInstToInst _ (SPushOnStack val) = Right (Push (sValToValueData val))
sInstToInst _ SCall = Right Call
sInstToInst _ SRet = Right Ret
sInstToInst insts (SJumpIfFalse val) =
    case getLabelIndexInInst insts val of 
        Nothing -> Left ("JumpIfFalse label not found: " ++ val)
        Just idx -> Right (JumpIfFalse idx)
sInstToInst insts (SJump val) =
    case getLabelIndexInInst insts val of 
        Nothing -> Left ("Jump label not found: " ++ val)
        Just idx -> Right (Jump idx)
sInstToInst _ (SPushArgOnStack val) = Right (PushArgOnStack val)
sInstToInst _ SPushStackOnArg = Right PushStackOnArg
sInstToInst _ SPopArg = Right PopArg
sInstToInst _ SPopStack = Right PopStack

sExprToInstsReplacingLabels :: SExpr -> SExpr -> Either String Insts
sExprToInstsReplacingLabels _ [] = Right []
sExprToInstsReplacingLabels s (SLabel _:as) = sExprToInstsReplacingLabels s as
sExprToInstsReplacingLabels s (SInstruction val:as) =
    case sInstToInst s val of
        Left err -> Left err
        Right res -> case sExprToInstsReplacingLabels s as of 
            Left err -> Left err
            Right end -> Right (res:end)

sExprToInsts :: SExpr -> Either String Insts
sExprToInsts sinsts = sExprToInstsReplacingLabels sinsts sinsts