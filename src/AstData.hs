data AstDeclaration = FuncDeclaration { args :: [Symbol], body :: [Ast]} deriving Show

data AstAssignation = VarAssignation { key :: Symbol, value :: Ast} deriving Show

newtype AstCall = FuncCall { args :: [Ast]} deriving Show

data Ast =
  Symbol String |
  Boolean Bool |
  Integer Int |
  Str String |
  List [Ast] |
  Declaration AstDeclaration |
  Assignation AstAssignation |
  Call AstCall
    deriving Show
