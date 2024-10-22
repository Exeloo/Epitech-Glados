module StackData (ValueData(..), Stack) where

data ValueData = VInt Int | VBool Bool
    deriving (Show, Eq)

type Stack = [ValueData]
