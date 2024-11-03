module ExecInstructions (exec, takeListEnd) where

import InstructionData (InstructionData(..), Insts, ValueData(..), Stack, SysCall(..), Args)
import Data.Fixed (mod')
import Data.Maybe (fromMaybe)

takeListEnd :: [a] -> Int -> [a]
takeListEnd [] _ = []
takeListEnd list 0 = list
takeListEnd (_:as) idx = takeListEnd as (idx - 1)

insertOrUpdate :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
insertOrUpdate key new arr = (key, new) : arrWithoutKey
    where arrWithoutKey = filter ((/= key) . fst) arr

exec :: String -> Insts -> Args -> Insts -> Stack -> Either String String
exec res _ _ (Ret:_) _ = Right res
exec res insts args ((Push a):as) stack = exec res insts args as (a:stack)
exec res insts args (Call:as) ((VCall Add):(VInt a):(VInt b):s) = exec res insts args as (VInt (a + b):s)
exec res insts args (Call:as) ((VCall Add):(VDouble a):(VDouble b):s) = exec res insts args as (VDouble (a + b):s)
exec _ _ _ (Call:_) ((VCall Add):_) = Left "Add need two numbers"
exec res insts args (Call:as) ((VCall Sub):(VInt a):(VInt b):s) = exec res insts args as (VInt (a - b):s)
exec res insts args (Call:as) ((VCall Sub):(VDouble a):(VDouble b):s) = exec res insts args as (VDouble (a - b):s)
exec _ _ _ (Call:_) ((VCall Sub):_) = Left "Sub need two numbers"
exec res insts args (Call:as) ((VCall Mul):(VInt a):(VInt b):s) = exec res insts args as (VInt (a * b):s)
exec res insts args (Call:as) ((VCall Mul):(VDouble a):(VDouble b):s) = exec res insts args as (VDouble (a * b):s)
exec _ _ _ (Call:_) ((VCall Mul):_) = Left "Mul need two numbers"
exec _ _ _ (Call:_) ((VCall Div):(VInt _):(VInt 0):_) = Left "Division by 0"
exec _ _ _ (Call:_) ((VCall Div):(VDouble _):(VDouble 0):_) = Left "Division by 0"
exec res insts args (Call:as) ((VCall Div):(VInt a):(VInt b):s) = exec res insts args as (VInt (div a b):s)
exec res insts args (Call:as) ((VCall Div):(VDouble a):(VDouble b):s) = exec res insts args as (VDouble (a / b):s)
exec _ _ _ (Call:_) ((VCall Mod):(VInt _):(VInt 0):_) = Left "Modulo by 0"
exec _ _ _ (Call:_) ((VCall Mod):(VDouble _):(VDouble 0):_) = Left "Modulo by 0"
exec res insts args (Call:as) ((VCall Mod):(VInt a):(VInt b):s) = exec res insts args as (VInt (mod a b):s)
exec res insts args (Call:as) ((VCall Mod):(VDouble a):(VDouble b):s) = exec res insts args as (VDouble (mod' a b):s)
exec _ _ _ (Call:_) ((VCall Div):_) = Left "Div need two numbers"
exec res insts args (Call:as) ((VCall Eq):a:b:s) = exec res insts args as (VBool (a == b):s)
exec _ _ _ (Call:_) ((VCall Eq):_) = Left "Eq need two value to compare"
exec res insts args (Call:as) ((VCall Less):(VInt a):(VInt b):s) = exec res insts args as (VBool (a < b):s)
exec res insts args (Call:as) ((VCall Less):(VDouble a):(VDouble b):s) = exec res insts args as (VBool (a < b):s)
exec _ _ _ (Call:_) ((VCall Less):_) = Left "Less need two numbers"
exec res insts args (Call:as) ((VCall Not):(VInt a):s) = exec res insts args as (VBool (a == 0):s)
exec res insts args (Call:as) ((VCall Not):(VBool a):s) = exec res insts args as (VBool (not a):s)
exec res insts args (Call:as) ((VCall Not):(VDouble a):s) = exec res insts args as (VBool (a == 0.0):s)
exec res insts args (Call:as) ((VCall Not):(VString a):s) = exec res insts args as (VBool (null a):s)
exec res insts args (Call:as) ((VCall Not):(VArray a):s) = exec res insts args as (VBool (null a):s)
exec res insts args (Call:as) ((VCall Not):(VObject a):s) = exec res insts args as (VBool (null a):s)
exec res insts args (Call:as) ((VCall Not):VUndefined:s) = exec res insts args as (VBool True:s)
exec _ _ _ (Call:_) ((VCall Not):_) = Left "Not need a value to be call"
exec res insts args (Call:as) ((VCall Or):(VBool a):(VBool b):s) = exec res insts args as (VBool (a || b):s)
exec _ _ _ (Call:_) ((VCall Or):_) = Left "Or need two bool to compare"
exec res insts args (Call:as) ((VCall And):(VBool a):(VBool b):s) = exec res insts args as (VBool (a && b):s)
exec _ _ _ (Call:_) ((VCall And):_) = Left "And need two bool to compare"
exec res insts args (Call:as) ((VCall Access):VInt idx:VArray arr:s) = exec res insts args as (fromMaybe VUndefined (lookup idx arr):s)
exec res insts args (Call:as) ((VCall Access):VString str:VObject obj:s) = exec res insts args as (fromMaybe VUndefined (lookup str obj):s)
exec res insts args (Call:as) ((VCall Modify):new:VInt idx:VArray arr:s) = exec res insts args as (VArray (insertOrUpdate idx new arr):s)
exec res insts args (Call:as) ((VCall Modify):new:VString str:VObject obj:s) = exec res insts args as (VObject (insertOrUpdate str new obj):s)
exec res insts args (Call:as) ((VCall Print):(VInt a):s) = exec (res ++ show a) insts args as s
exec res insts args (Call:as) ((VCall Print):(VBool a):s) = exec (res ++ show a) insts args as s
exec res insts args (Call:as) ((VCall Print):(VDouble a):s) = exec (res ++ show a) insts args as s
exec res insts args (Call:as) ((VCall Print):(VString a):s) = exec (res ++ a) insts args as s
exec res insts args (Call:as) ((VCall Print):(VArray a):s) = exec (res ++ show a) insts args as s
exec res insts args (Call:as) ((VCall Print):(VObject a):s) = exec (res ++ show a) insts args as s
exec res insts args (Call:as) ((VCall Print):VUndefined:s) = exec (res ++ "Undefined") insts args as s
exec _ _ _ (Call:_) ((VCall Print):_) = Left "Print need a value to be call"
exec _ _ _ (Call:_) _ = Left "Can't Call on empty stack"

exec res insts args ((JumpIfFalse val):_) ((VBool False):s) =
    if length insts < val
        then Left "Can't jump more instructions than available"
        else exec res insts args (takeListEnd insts val) s
exec res insts args ((JumpIfFalse _):as) ((VBool True):s) = exec res insts args as s
exec _ _ _ ((JumpIfFalse _):_) _ = Left "JumpIfFalse need a boolean"
exec res insts args ((Jump val):_) stack =
    if length insts < val
        then Left "Can't jump more instructions than available"
        else exec res insts args (takeListEnd insts val) stack
exec res insts args ((PushArgOnStack val):as) stack =
    if val >= length args
        then Left "Index is bigger than args number"
        else exec res insts args as ((args!!val):stack)
exec res insts args (PushStackOnArg:as) (a:s) = exec res insts (a:args) as s
exec _ _ _ (PushStackOnArg:_) [] = Left "Can't push empty stack on arg"
exec res insts (_:args) (PopArg:as) stack = exec res insts args as stack
exec _ _ [] (PopArg:_) _ = Left "Can't pop empty arg"
exec res insts args (PopStack:as) (_:stack) = exec res insts args as stack
exec _ _ _ (PopStack:_) [] = Left "Can't pop empty stack"
exec _ _ _ _ _ = Left "No Ret at end of instructions"
