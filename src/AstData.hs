data Function = Func {funcName :: String, funcArgs :: [Ast], funcBody :: [Ast]} | Call {callFunc :: String, callArgs :: Ast} deriving Show

data Ast =  Str String | Lst [Ast] | Inte Int | Sym String | Boolean Bool | FuncAst Function deriving Show
