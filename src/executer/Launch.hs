module Launch (launch, exec) where

import InstructionData(InstructionData(..), Insts, InstCall(..))
import StackData(ValueData(..), Stack)
import ArgData (Args)

deleteIndex :: Int -> [a] -> [a]
deleteIndex _ [] = []
deleteIndex 0 (a:as) = as
deleteIndex idx (a:as) = a:deleteIndex (idx - 1) as

exec :: Args -> Insts -> Stack -> Either String ValueData
exec args ((Push a):as) stack = exec args as (a:stack)
exec args ((Call Add):as) ((VInt a):(VInt b):s) = exec args as (VInt (a + b):s)
exec _ ((Call Add):_) stack = Left (if length stack < 2 then "Add need two arguments" else "Add need two number")
exec args ((Call Sub):as) ((VInt a):(VInt b):s) = exec args as (VInt (a - b):s)
exec _ ((Call Sub):_) stack = Left (if length stack < 2 then "Sub need two arguments" else "Sub need two number")
exec args ((Call Mul):as) ((VInt a):(VInt b):s) = exec args as (VInt (a * b):s)
exec _ ((Call Mul):_) stack = Left (if length stack < 2 then "Mul need two arguments" else "Mul need two number")
exec _ ((Call Div):_) ((VInt _):(VInt 0):_) = Left "Division by 0"
exec args ((Call Div):as) ((VInt a):(VInt b):s) = exec args as (VInt (div a b):s)
exec _ ((Call Div):_) stack = Left (if length stack < 2 then "Div need two arguments" else "Div need two number")
exec args ((Call Eq):as) ((VInt a):(VInt b):s) = exec args as (VBool (a == b):s)
exec _ ((Call Eq):_) stack = Left (if length stack < 2 then "Eq need two arguments" else "Eq need two number")
exec args ((Call Less):as) ((VInt a):(VInt b):s) = exec args as (VBool (a < b):s)
exec _ ((Call Less):_) stack = Left (if length stack < 2 then "Less need two arguments" else "Less need two number")
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
