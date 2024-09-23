module AstData () where

data AstDeclaration = FuncDeclaration { declareArgs :: [Symbol], declareBody :: [Ast] } deriving Show

data AstAssignation = VarAssignation { assignationKey :: Symbol, assignationValue :: Ast } deriving Show

type Symbol = String

data FuncArg = Func AstDeclaration | Sym Symbol deriving Show

data AstCall = FuncCall { callFunction :: FuncArg, callArgs :: [Ast] } deriving Show

data Ast =
  Symb Symbol |
  Boolean Bool |
  Integer Int |
  Str String |
  List [Ast] |
  Declaration AstDeclaration |
  Assignation AstAssignation |
  Call AstCall
    deriving Show
