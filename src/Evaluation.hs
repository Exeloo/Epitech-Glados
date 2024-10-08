{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Evaluation
-}

module Evaluation where

import AstData
import Symbol
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

globalVar :: IORef [[Ast]]
globalVar = unsafePerformIO (newIORef [])

writeGlobalVar :: [[Ast]] -> [[Ast]]
writeGlobalVar x = unsafePerformIO (modifyIORef globalVar (const x) >> return x)

readGlobalVar :: IO [[Ast]]
readGlobalVar = readIORef globalVar >>= \x -> return x

checkElemList :: Symbol -> [[Ast]] -> Either String Bool
checkElemList x var = checkElemList' False x var

checkElemList' :: Bool -> Symbol -> [[Ast]] -> Either String Bool
-- checkElemList' False x _ = checkElemList x (readGlobalVar)
checkElemList' False x varinit = unsafePerformIO $ do
    var <- readGlobalVar
    return $ if var == [] then checkElemList' True x varinit else checkElemList' True x var
checkElemList' True _ [] = Left "Not existing"
checkElemList' True x (y:ys) = case (findAssignation x y) of
                    Nothing -> checkElemList' True x ys
                    Just _ -> Right True

findAssignation :: Symbol -> [Ast] -> Maybe Ast
findAssignation _ [] = Nothing
findAssignation x ((AAssignation (VarAssignation a b)):xs) | x == a = Just (AAssignation (VarAssignation a b))
                                                           | otherwise = (findAssignation x xs)
findAssignation x (_:ys) = findAssignation x ys

getElemList :: Symbol -> [[Ast]] -> Either String Ast
getElemList x var = getElemList' False x var

getElemList' :: Bool -> Symbol -> [[Ast]] -> Either String Ast
getElemList' False x varinit = unsafePerformIO $ do
    var <- readGlobalVar
    return $ if var == [] then getElemList' True x varinit else getElemList' True x var
getElemList' True _ [] = Left "No useful data"
getElemList' True x (xs:rest) = case findAssignation x xs of
    Nothing -> getElemList' True x rest
    Just a -> Right a

addAssignation :: Ast -> [[Ast]] -> [[Ast]]
addAssignation x var = addAssignation' False x var

addAssignation' :: Bool -> Ast -> [[Ast]] -> [[Ast]]
addAssignation' False x _ = unsafePerformIO $ do
    var <- readGlobalVar
    return $ addAssignation' True x var
addAssignation' True x [] = writeGlobalVar [[x]]
addAssignation' True x xs = writeGlobalVar ((init xs) ++ [(last xs) ++ [x]])
