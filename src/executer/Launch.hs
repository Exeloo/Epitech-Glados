module Launch (launch, exec) where

import InstructionData(InstructionData(..), Insts)
import StackData(ValueData(..), Stack, InstCall(..))

exec :: Insts -> Stack -> Either String ValueData
exec ((IPush a):as) stack = exec as (a:stack)
exec (ICall:as) ((VCall Add):(VInt a):(VInt b):s) = exec as (VInt (a + b):s)
exec (ICall:_) (VCall Add:_) = Left "Add need two number"
exec (ICall:as) ((VCall Sub):(VInt a):(VInt b):s) = exec as (VInt (a - b):s)
exec (ICall:_) (VCall Sub:_) = Left "Sub need two number"
exec (ICall:as) ((VCall Mul):(VInt a):(VInt b):s) = exec as (VInt (a * b):s)
exec (ICall:_) (VCall Mul:_) = Left "Mul need two number"
exec (ICall:_) ((VCall Div):(VInt _):(VInt 0):_) = Left "Division by 0"
exec (ICall:as) ((VCall Div):(VInt a):(VInt b):s) = exec as (VInt (div a b):s)
exec (ICall:_) (VCall Div:_) = Left "Div need two number"
exec (ICall:as) ((VCall Eq):(VInt a):(VInt b):s) = exec as (VBool (a == b):s)
exec (ICall:_) (VCall Eq:_) = Left "Eq need two number"
exec (ICall:as) ((VCall Less):(VInt a):(VInt b):s) = exec as (VBool (a < b):s)
exec (ICall:_) (VCall Less:_) = Left "Less need two number"
exec ((JumpIfFalse val):as) ((VBool False):s) = exec (reverse (take val (reverse as))) s
exec ((JumpIfFalse val):as) ((VBool True):s) = exec as s
exec ((JumpIfFalse _):_) stack = Left "JumpIfFalse need a boolean"
exec (IRet:_) (a:_) = Right a
exec (IRet:_) [] = Left "Ret need a value"
exec (a:_) _ = Left ("Unkown Instruction :" ++ show a)
exec _ _ = Left "No Ret at end of instructions"

launch :: [String] -> IO Bool
launch [] = print (exec [] []) >> return False
