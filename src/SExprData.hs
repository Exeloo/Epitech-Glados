{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- SExpData
-}

module SExprData (SExpr(..)) where

import Symbol

type Separator = Char

type Body = [SExpr]

data SExpr =
   SInt Int |
   SFloat Float |
   SBool Bool |
   SString String |
   SSymbol Symbol |
   SArray [SExpr] |
   SBracket [(Body, Separator)] |
   SLine [SExpr]

instance Show SExpr where
  show (SInt x) = "Integer: " ++ show x
  show (SFloat x) = "Float: " ++ show x
  show (SBool x) = "Boolean: " ++ show x
  show (SString x) = "String: \"" ++ x ++ "\""
  show (SSymbol x) = "Symbol: " ++ show x
  show (SArray x) = "Array: [" ++ foldl (\a b -> a ++ (if null a then "" else ", ") ++ show b) [] x ++ "]"
  show (SBracket x) = "Bracket: {" ++ foldl (\a (b, sep) -> a ++ (if null a then "" else " | ") ++ "[" ++ foldl (\a' b' -> a' ++ (if null a' then "" else ", ") ++ show b') [] b ++ "]" ++ [sep]) [] x ++ "}"
  show (SLine x) = "Line: [" ++ foldl (\a b -> a ++ (if null a then "" else ", ") ++ show b) [] x ++ "]"

instance Eq SExpr where
  (SInt x1) == (SInt x2) = x1 == x2
  (SFloat f1) == (SFloat f2) = f1 == f2
  (SBool b1) == (SBool b2) = b1 == b2
  (SString str1) == (SString str2) = str1 == str2
  (SSymbol s1) == (SSymbol s2) = s1 == s2
  (SArray a1) == (SArray a2) = a1 == a2
  (SBracket br1) == (SBracket br2) = br1 == br2
  (SLine l1) == (SLine l2) = l1 == l2
  _ == _ = False


