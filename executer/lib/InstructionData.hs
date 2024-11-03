module InstructionData (InstructionData(..), Insts, ValueData(..), Stack, SysCall(..), Args) where

data SysCall = Add | Sub | Mul | Div | Mod | Eq | Less | Not | Or | And | Access | Modify | Print
    deriving (Show, Eq)

data ValueData = VInt Int | VBool Bool | VDouble Double | VString String | VArray [(Int, ValueData)] | VObject [(String, ValueData)] | VCall SysCall | VUndefined
    deriving (Show, Eq)

type Stack = [ValueData]

type Args = [ValueData]

data InstructionData = Push ValueData | Call | Ret | JumpIfFalse Int | PushArgOnStack Int | PushStackOnArg | PopArg | PopStack | Jump Int
    deriving (Show, Eq)

type Insts = [InstructionData]
