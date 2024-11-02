module SExprData (SSysCall(..), SValue(..), SInst(..), SAsm(..), SExpr) where

data SSysCall = SAdd | SSub | SMul | SDiv | SEq | SLess | SNot | SOr | SAnd | SAccessArray | SModifyArray | SAccessObject | SModifyObject | SPrint
    deriving (Show, Eq)

data SValue = SInt Int | SBool Bool | SDouble Double | SString String | SArray [SValue] | SObject [(String, SValue)] | SValueCall SSysCall | SUndefined
    deriving (Show, Eq)

data SInst = SPushOnStack SValue | SCall | SRet | SJumpIfFalse String | SPushArgOnStack Int | SPushStackOnArg | SPopArg | SPopStack | SJump String
    deriving (Show, Eq)

data SAsm = SInstruction SInst | SLabel String
    deriving (Show, Eq)

type SExpr = [SAsm]
