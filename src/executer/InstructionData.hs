module InstructionData(InstructionData(..), Insts, InstCall(..)) where

import StackData(ValueData(..))

data InstCall = Add | Sub | Mul | Div | Eq | Less | JumpIfFalse
    deriving (Show, Eq)

data InstructionData = IPush ValueData | ICall InstCall | IRet
    deriving (Show, Eq)

type Insts = [InstructionData]
