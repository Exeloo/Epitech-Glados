module InstructionData (InstructionData(..), Insts, ValueData(..), Stack, InstCall(..), Args) where

data InstCall = Add | Sub | Mul | Div | Eq | Less
    deriving (Show, Eq)

data ValueData = VInt Int | VBool Bool | VDouble Double | VString String | VArray [ValueData] | VCall InstCall
    deriving (Show, Eq)

type Stack = [ValueData]

type Args = [ValueData]

data InstructionData = Push ValueData | Call | Ret | JumpIfFalse Int | PushArgOnStack Int | PushStackOnArg | PopArg | PopStack | Jump Int
    deriving (Show, Eq)

type Insts = [InstructionData]
