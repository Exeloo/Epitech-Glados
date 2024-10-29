module SExprData (SInstCall(..), SValue(..), SInst(..), SAsm(..), SExpr) where

data SInstCall = SAdd | SSub | SMul | SDiv | SEq | SLess
    deriving (Show, Eq)

data SValue = SInt Int | SBool Bool | SDouble Double | SString String | SArray [SValue] | SValueCall SInstCall
    deriving (Show, Eq)

data SInst = SPushOnStack SValue | SCall | SRet | SJumpIfFalse String | SPushArgOnStack Int | SPushStackOnArg | SPopArg | SPopStack | SJump String
    deriving (Show, Eq)

data SAsm = SInstruction SInst | SLabel String
    deriving (Show, Eq)

type SExpr = [SAsm]
