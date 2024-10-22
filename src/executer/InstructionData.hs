module InstructionData(InstructionData(..), Insts) where

import StackData(ValueData(..))

data InstructionData = Push ValueData | Call | Ret | JumpIfFalse Int | PushArg Int
    deriving (Show, Eq)

type Insts = [InstructionData]

