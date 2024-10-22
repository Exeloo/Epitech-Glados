module Launch (launch, exec) where

import InstructionData(InstructionData(..), Insts, InstCall(..))
import StackData(ValueData(..), Stack)

exec :: Insts -> Stack -> Either String ValueData
exec ((IPush a):as) stack = exec as (a:stack)
exec ((ICall Add):as) ((VInt a):(VInt b):s) = exec as (VInt (a + b):s)
exec ((ICall Add):_) stack = Left (if length stack < 2 then "Add need two arguments" else "Add need two number")
exec ((ICall Sub):as) ((VInt a):(VInt b):s) = exec as (VInt (a - b):s)
exec ((ICall Sub):_) stack = Left (if length stack < 2 then "Sub need two arguments" else "Sub need two number")
exec ((ICall Mul):as) ((VInt a):(VInt b):s) = exec as (VInt (a * b):s)
exec ((ICall Mul):_) stack = Left (if length stack < 2 then "Mul need two arguments" else "Mul need two number")
exec ((ICall Div):_) ((VInt _):(VInt 0):_) = Left "Division by 0"
exec ((ICall Div):as) ((VInt a):(VInt b):s) = exec as (VInt (div a b):s)
exec ((ICall Div):_) stack = Left (if length stack < 2 then "Div need two arguments" else "Div need two number")
exec ((ICall Eq):as) ((VInt a):(VInt b):s) = exec as (VBool (a == b):s)
exec ((ICall Eq):_) stack = Left (if length stack < 2 then "Eq need two arguments" else "Eq need two number")
exec ((ICall Less):as) ((VInt a):(VInt b):s) = exec as (VBool (a < b):s)
exec ((ICall Less):_) stack = Left (if length stack < 2 then "Less need two arguments" else "Less need two number")
exec ((JumpIfFalse val):as) ((VBool False):s) = exec (reverse (take val (reverse as))) s
exec ((JumpIfFalse val):as) ((VBool True):s) = exec as s
exec ((JumpIfFalse _):_) stack = Left "JumpIfFalse need a boolean"
exec (IRet:_) (a:_) = Right a
exec (IRet:_) [] = Left "Ret need a value"
exec (a:_) _ = Left ("Unkown Instruction :" ++ show a)
exec _ _ = Left "No Ret at end of instructions"

launch :: [String] -> IO Bool
launch [] = print (exec [] []) >> return False
