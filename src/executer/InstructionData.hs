module InstructionData(InstructionData(..), Insts) where

import StackData(ValueData(..), InstCall(..))

data InstructionData = IPush ValueData | ICall | IRet | JumpIfFalse Int
    deriving (Show, Eq)

type Insts = [InstructionData]
