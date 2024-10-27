module Launch (launch, exec) where

import InstructionData(InstructionData(..), Insts)
import StackData(ValueData(..), Stack, InstCall(..))
import ArgData (Args)

deleteIndex :: Int -> [a] -> [a]
deleteIndex _ [] = []
deleteIndex 0 (a:as) = as
deleteIndex idx (a:as) = a:deleteIndex (idx - 1) as

exec :: Args -> Insts -> Stack -> Either String ValueData
exec args ((Push a):as) stack = exec args as (a:stack)
exec args ((Call):as) ((VCall Add):(VInt a):(VInt b):s) = exec args as (VInt (a + b):s)
exec _ ((Call):_) ((VCall Add):_) = Left "Add need two number"
exec args ((Call):as) ((VCall Sub):(VInt a):(VInt b):s) = exec args as (VInt (a - b):s)
exec _ ((Call):_) ((VCall Sub):_) = Left "Sub need two number"
exec args ((Call):as) ((VCall Mul):(VInt a):(VInt b):s) = exec args as (VInt (a * b):s)
exec _ ((Call):_) ((VCall Mul):_) = Left "Mul need two number"
exec _ ((Call):_) ((VCall Div):(VInt _):(VInt 0):_) = Left "Division by 0"
exec args ((Call):as) ((VCall Div):(VInt a):(VInt b):s) = exec args as (VInt (div a b):s)
exec _ ((Call):_) ((VCall Div):_) = Left "Div need two number"
exec args ((Call):as) ((VCall Eq):(VInt a):(VInt b):s) = exec args as (VBool (a == b):s)
exec _ ((Call):_) ((VCall Eq):_) = Left "Eq need two number"
exec args ((Call):as) ((VCall Less):(VInt a):(VInt b):s) = exec args as (VBool (a < b):s)
exec _ ((Call):_) ((VCall Less):_) = Left "Less need two number"
exec args ((JumpIfFalse val):as) ((VBool False):s) = if length as < val then Left "Can't jump more instructions than available" else exec args (reverse (take (length as - val) (reverse as))) s
exec args ((JumpIfFalse val):as) ((VBool True):s) = exec args as s
exec _ ((JumpIfFalse _):_) _ = Left "JumpIfFalse need a boolean"
exec args ((PushArg val):as) stack = if val >= length args then Left "Index is bigger than Args number" else exec args as ((args!!val):stack)
exec _ (Ret:_) [] = Left "Ret need a value"
exec _ (Ret:_) (a:_) = Right a
exec _ (a:_) _ = Left ("Unkown Instruction :" ++ show a)
exec _ _ _ = Left "No Ret at end of instructions"

launch :: [String] -> IO Bool
launch [] = print (exec [] [] []) >> return False
