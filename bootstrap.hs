module Main (main) where

data SExpr = Integer Int | List [SExpr] | Symbol String

instance Show SExpr where
    show (Integer x) = "Integer : " ++ show x ++ " "
    show (Symbol x) = "Symbol : " ++ show x ++ " "
    show (List xs) = "List : [" ++ unwords (map show xs) ++ "]"

getInteger :: SExpr -> Maybe Int
getInteger (Integer x) = Just x
getInteger _ = Nothing

getSymbol :: SExpr -> Maybe String
getSymbol (Symbol x) = Just x
getSymbol _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (List x) = Just x
getList _ = Nothing

data Ast = Define {defineKey :: String, defineValue :: Ast} | Call {callFunc :: String, callArgs :: Ast} | Lst [Ast] | Inte Int | Sym String | Boolean Bool deriving Show

sexprToAST :: [String] -> SExpr -> Maybe Ast
sexprToAST _ (Integer x) = Just (Inte x)
sexprToAST _ (Symbol "#t") = Just (Boolean True)
sexprToAST _ (Symbol "#f") = Just (Boolean False)
sexprToAST _ (Symbol x) = Just (Sym x)
sexprToAST call (List ((Symbol "define"):(Symbol key):value)) =
    case (mapM (sexprToAST (call++[key])) value) of
        Just (b:[]) -> Just (Define {defineKey = key, defineValue = b})
        _ -> Nothing
sexprToAST call (List ((Symbol s):args)) | s `elem` call = mapM (sexprToAST call) args >>= \lstAst ->
                    Just (Call {callFunc = s, callArgs = Lst lstAst})
sexprToAST call (List x) = mapM (sexprToAST call) x >>= \lstAst ->
                        Just (Lst lstAst)

callAST :: String -> Ast -> Maybe Ast
callAST "+" (Lst [Inte x, Inte y]) = Just (Inte (x + y))
callAST "-" (Lst [Inte x, Inte y]) = Just (Inte (x - y))
callAST "*" (Lst [Inte x, Inte y]) = Just (Inte (x * y))
callAST "/" (Lst [Inte x, Inte y]) = Just (Inte (x `div` y))
callAST a b = Just (Lst [Sym a, b])

evalAST :: Ast-> Maybe Ast
evalAST (Inte x) = Just (Inte x)
evalAST (Sym x) = Just (Sym x)
evalAST (Define {defineKey = key, defineValue = value}) = Just (Define {defineKey = key, defineValue = value})
evalAST (Call func (Lst args)) = case mapM evalAST args of
    Just x -> case callAST func (Lst x) of
        Just x -> Just x
        Nothing -> Nothing
    Nothing -> Nothing
evalAST (Lst x) = case mapM evalAST x of
    Just x -> Just (Lst x)
    Nothing -> Nothing


main :: IO ()
main = case evalAST (Call {callFunc = "*", callArgs = Lst [Inte 6, Call {callFunc = "+", callArgs = Lst [Inte 4, Inte 3]}]}) of
        Just x -> putStrLn (show x)
        Nothing -> putStrLn "Nothing"
-- main = case sexprToAST ["+", "-", "*", "/"] (List [(Symbol "define"), (Symbol "x"), (List [(Symbol "a"), (Symbol "b")]), (Symbol "+", )]) of
--         Just x -> putStrLn (show x)
--         Nothing -> putStrLn "Nothing"
-- main = case sexprToAST (List [(Symbol "define"), (Symbol "x"), (Integer 5)]) of
--         Just x -> putStrLn (show x)
--         Nothing -> putStrLn "Nothing"
-- main = case sexprToAST (Symbol "here") of
--         Just x -> putStrLn (show x)
--         Nothing -> putStrLn "Nothing"
-- main = putStrLn (show (List [(Symbol "define"), (Symbol "x"), (Integer 5)]))
-- main = putStrLn (show (List [(Integer 42), (Symbol "Quoi"), (List [(Integer 24), (Symbol "Foo")])]))
-- main = case getList (List [(Integer 42), (Symbol "Quoi"), (List [(Integer 24), (Symbol "Foo")])]) of
--         Just x -> putStrLn (show x)
--         Nothing -> putStrLn "Nothing"
-- main = case getSymbol (Symbol "Feur") of
--         Just x -> putStrLn (show x)
--         Nothing -> putStrLn "Nothing"
-- main = case getSymbol(Integer 5) of
--         Just x -> putStrLn (show x)
--         Nothing -> putStrLn "Nothing"
-- main = case getInteger (Integer 5) of
--         Just x -> putStrLn (show x)
--         Nothing -> putStrLn "Nothing"
