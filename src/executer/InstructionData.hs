module InstructionData(InstructionData(..), Insts, InstCall(..)) where

import StackData(ValueData(..))

data InstCall = Add | Sub | Mul | Div | Eq | Less
    deriving (Show, Eq)

data InstructionData = Push ValueData | Call InstCall | Ret | JumpIfFalse Int | PushArg Int
    deriving (Show, Eq)

type Insts = [InstructionData]

