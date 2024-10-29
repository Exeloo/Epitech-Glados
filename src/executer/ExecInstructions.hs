module ExecInstructions (exec) where

import InstructionData (InstructionData(..), Insts, ValueData(..), Stack, InstCall(..), Args)

takeListEnd :: [a] -> Int -> [a]
takeListEnd [] _ = []
takeListEnd list 0 = list
takeListEnd (_:as) idx = takeListEnd as (idx - 1)

exec :: Insts -> Args -> Insts -> Stack -> Either String ValueData
exec insts args ((Push a):as) stack = exec insts args as (a:stack)
exec insts args (Call:as) ((VCall Add):(VInt a):(VInt b):s) = exec insts args as (VInt (a + b):s)
exec _ _ (Call:_) ((VCall Add):_) = Left "Add need two number"
exec insts args (Call:as) ((VCall Sub):(VInt a):(VInt b):s) = exec insts args as (VInt (a - b):s)
exec _ _ (Call:_) ((VCall Sub):_) = Left "Sub need two number"
exec insts args (Call:as) ((VCall Mul):(VInt a):(VInt b):s) = exec insts args as (VInt (a * b):s)
exec _ _ (Call:_) ((VCall Mul):_) = Left "Mul need two number"
exec _ _ (Call:_) ((VCall Div):(VInt _):(VInt 0):_) = Left "Division by 0"
exec insts args (Call:as) ((VCall Div):(VInt a):(VInt b):s) = exec insts args as (VInt (div a b):s)
exec _ _ (Call:_) ((VCall Div):_) = Left "Div need two number"
exec insts args (Call:as) ((VCall Eq):(VInt a):(VInt b):s) = exec insts args as (VBool (a == b):s)
exec _ _ (Call:_) ((VCall Eq):_) = Left "Eq need two number"
exec insts args (Call:as) ((VCall Less):(VInt a):(VInt b):s) = exec insts args as (VBool (a < b):s)
exec _ _ (Call:_) ((VCall Less):_) = Left "Less need two number"
exec insts args ((JumpIfFalse val):_) ((VBool False):s) =
    if length insts < val
        then Left "Can't jump more instructions than available"
        else exec insts args (takeListEnd insts val) s
exec insts args ((JumpIfFalse _):as) ((VBool True):s) = exec insts args as s
exec _ _ ((JumpIfFalse _):_) _ = Left "JumpIfFalse need a boolean"
exec insts args ((Jump val):_) stack =
    if length insts < val
        then Left "Can't jump more instructions than available"
        else exec insts args (takeListEnd insts val) stack
exec insts args ((PushArgOnStack val):as) stack =
    if val >= length args
        then Left "Index is bigger than args number"
        else exec insts args as ((args!!val):stack)
exec insts args (PushStackOnArg:as) (a:s) = exec insts (a:args) as s
exec _ _ (PushStackOnArg:_) [] = Left "Can't push empty stack on arg"
exec insts (_:args) (PopArg:as) stack = exec insts args as stack
exec _ [] (PopArg:_) _ = Left "Can't pop empty arg"
exec insts args (PopStack:as) (_:stack) = exec insts args as stack
exec _ _ (PopStack:_) [] = Left "Can't pop empty stack"
exec _ _ (Ret:_) [] = Left "Ret need a value"
exec _ _ (Ret:_) (a:_) = Right a
exec _ _ (a:_) _ = Left ("Unkown Instruction :" ++ show a)
exec _ _ _ _ = Left "No Ret at end of instructions"
