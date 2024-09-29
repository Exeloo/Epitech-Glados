{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- Evaluation
-}

module Evaluation where

import AstData

checkElemList :: Ast -> [[Ast]] -> Either String Bool
checkElemList _ [] = Left "Not existing"
checkElemList x (y:ys) | (elem x y) == True = Right True
                       | otherwise = checkElemList x ys

