{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- Evaluation
-}

module Evaluation where

import AstData
import Symbol

checkElemList :: Symbol -> [[Ast]] -> Either String Bool
checkElemList _ [] = Left "Not existing"
checkElemList x (y:ys) = case (findAssignation x y) of
                    Nothing -> checkElemList x ys
                    Just _ -> Right True

findAssignation :: Symbol -> [Ast] -> Maybe Ast
findAssignation _ [] = Nothing
findAssignation x ((AAssignation (VarAssignation a b)):xs) | x == a = Just (AAssignation (VarAssignation a b))
                                                           | otherwise = (findAssignation x xs)
findAssignation x (_:ys) = findAssignation x ys

getElemList :: Symbol -> [[Ast]] -> Either String Ast
getElemList _ [] = Left "No usefull data"
getElemList x (xs) = case (findAssignation x (last xs)) of
                Nothing -> getElemList x (init xs)
                Just a -> Right a
