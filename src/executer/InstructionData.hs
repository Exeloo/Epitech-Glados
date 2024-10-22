module InstructionData(InstructionData(..), Insts, InstCall(..)) where

import StackData(ValueData(..))

data InstCall = Add | Sub | Mul | Div | Eq | Less
    deriving (Show, Eq)

data InstructionData = IPush ValueData | ICall InstCall | IRet | JumpIfFalse Int
    deriving (Show, Eq)

type Insts = [InstructionData]
