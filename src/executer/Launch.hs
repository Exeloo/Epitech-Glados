module Launch (launch, exec) where

import InstructionData(InstructionData(..), Insts, InstCall(..))
import StackData(ValueData(..), Stack)

execIPush :: ValueData -> Stack -> Either String Stack
execIPush val stack = Right (val:stack)

execICall :: InstCall -> Stack -> Either String Stack
execICall Add ((VInt a):(VInt b):s) = Right (VInt (a + b):s)
execICall Add stack = Left (if length stack < 2 then "Add need two arguments" else "Add need two number")
execICall Sub ((VInt a):(VInt b):s) = Right (VInt (a - b):s)
execICall Sub stack = Left (if length stack < 2 then "Sub need two arguments" else "Sub need two number")
execICall Mul ((VInt a):(VInt b):s) = Right (VInt (a + b):s)
execICall Mul stack = Left (if length stack < 2 then "Mul need two arguments" else "Mul need two number")
execICall Div ((VInt a):(VInt 0):s) = Left "Division by 0"
execICall Div ((VInt a):(VInt b):s) = Right (VInt (div a b):s)
execICall Div stack = Left (if length stack < 2 then "Div need two arguments" else "Div need two number")
execICall Eq ((VInt a):(VInt b):s) = Right (VBool (a == b):s)
execICall Eq stack = Left (if length stack < 2 then "Eq need two arguments" else "Eq need two number")
execICall Less ((VInt a):(VInt b):s) = Right (VBool (a < b):s)
execICall Less stack = Left (if length stack < 2 then "Less need two arguments" else "Less need two number")
execICall JumpIfFalse ((VBool a):s) = Right (VBool (a < b):s)
execICall JumpIfFalse stack = Left (if null stack then "JumpIfFalse need one arguments" else "JumpIfFalse need one boolean")
execICall call _ = Left ("Unkown OP Code :" ++ (show call))

exec :: Insts -> Either String Stack -> Either String ValueData
exec [] (Left a) = Left a
exec ((IPush a):as) (Right stack) = exec as (execIPush a stack)
exec ((ICall a):as) (Right stack) = exec as (execICall a stack)
exec (IRet:_) (Right (a:_)) = Right a 
exec (a:_) _ = Left ("Unkown Instruction :" ++ (show a))
exec _ _ = Left "No Ret at end of instructions"

launch :: [String] -> IO Bool
launch [] = putStrLn (show (exec [] (Right []))) >> return False
