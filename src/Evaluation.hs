{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- Evaluation
-}

module Evaluation where

import AstData
import Symbol

getAssignationKeyList :: [Ast] -> [Symbol]
getAssignationKeyList [] = []
getAssignationKeyList ((AAssignation (VarAssignation a _)):xs) = a:(getAssignationKeyList xs)
getAssignationKeyList (_:xs) = getAssignationKeyList xs

checkElemList :: Symbol -> [[Ast]] -> Either String Bool
checkElemList _ [] = Left "Not existing"
checkElemList x (y:ys) | (elem x (getAssignationKeyList y)) == True = Right True
                       | otherwise = checkElemList x ys
