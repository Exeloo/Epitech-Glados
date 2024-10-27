module StackData (ValueData(..), Stack, InstCall(..)) where

data InstCall = Add | Sub | Mul | Div | Eq | Less
    deriving (Show, Eq)

data ValueData = VInt Int | VBool Bool | VCall InstCall
    deriving (Show, Eq)

type Stack = [ValueData]
