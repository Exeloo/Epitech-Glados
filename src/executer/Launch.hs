module Launch (launch, exec) where

import InstructionData(InstPush(..), InstCall(..), InstructionData(..), Insts, OpeAritData(..))
import StackData(ValueData(..), Stack)

execInstPush :: InstPush -> Stack -> Either String Stack
execInstPush (Push val) stack = Right (val:stack)

execInstCall :: InstCall -> Stack -> Either String Stack
execInstCall (Call Add) ((VInt a):(VInt b):s) = Right (VInt (a + b):s)
execInstCall (Call Add) stack = Left (if length stack < 2 then "Add need two arguments" else "Add need two number")
execInstCall (Call Sub) ((VInt a):(VInt b):s) = Right (VInt (a - b):s)
execInstCall (Call Sub) stack = Left (if length stack < 2 then "Sub need two arguments" else "Sub need two number")
execInstCall (Call Mul) ((VInt a):(VInt b):s) = Right (VInt (a + b):s)
execInstCall (Call Mul) stack = Left (if length stack < 2 then "Mul need two arguments" else "Mul need two number")
execInstCall (Call Div) ((VInt a):(VInt 0):s) = Left "Division by 0"
execInstCall (Call Div) ((VInt a):(VInt b):s) = Right (VInt (div a b):s)
execInstCall (Call Div) stack = Left (if length stack < 2 then "Div need two arguments" else "Div need two number")
execInstCall call _ = Left ("Unkown OP Code :" ++ (show call))

exec :: Insts -> Either String Stack -> Either String ValueData
exec [] (Left a) = Left a
exec ((IPush a):as) (Right stack) = exec as (execInstPush a stack)
exec ((ICall a):as) (Right stack) = exec as (execInstCall a stack)
exec (IRet:_) (Right (a:_)) = Right a 
exec (a:_) _ = Left ("Unkown Instruction :" ++ (show a))
exec _ _ = Left "No Ret at end of instructions"

launch :: [String] -> IO Bool
launch [] = putStrLn (show (exec [] (Right []))) >> return False
