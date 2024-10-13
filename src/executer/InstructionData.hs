module InstructionData (InstPush(..), InstCall(..), InstructionData(..), Insts, OpeAritData(..)) where

import StackData(ValueData(..))

data OpeAritData = Add | Sub | Mul | Div
    deriving (Show, Eq)

data InstCall = Call { op :: OpeAritData }
    deriving (Show, Eq)
data InstPush = Push { value :: ValueData }
    deriving (Show, Eq)

data InstructionData = IPush InstPush | ICall InstCall | IRet
    deriving (Show, Eq)

type Insts = [InstructionData]
