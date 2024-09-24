{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- SExpData
-}

module SExprData (SExpr(..)) where

import Symbol

data SExpr =
  SInt Int |
  SList [SExpr] |
  SSymbol Symbol |
  SString String

instance Show SExpr where
    show (SInt x) = "Integer: " ++ show x
    show (SSymbol x) = "Symbol: " ++ show x
    show (SList x) = "List: [" ++ foldl (\a b -> a ++ (if null a then "" else ", ") ++ show b) [] x ++ "]"
    show (SString x) = "String: \"" ++ x ++ "\""
  