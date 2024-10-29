{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- SExpData
-}

module SExprData (SExpr(..)) where

import Data.List (intercalate)

import Symbol

type Body = [SExpr]

data SExpr =
  SInt Int |
  SFloat Float |
  SBool Bool |
  SString String |
  SSymbol Symbol |
  SArray [SExpr] |
  SStruct [Body] |
  SParenthesis [SExpr] |
  SBracket [SExpr] |
  SLine [SExpr]

instance Show SExpr where
  show (SInt x) = "Integer: " ++ show x
  show (SFloat x) = "Float: " ++ show x
  show (SBool x) = "Boolean: " ++ show x
  show (SString x) = "String: \"" ++ x ++ "\""
  show (SSymbol x) = "Symbol: " ++ show x
  show (SArray x) = "Array: [" ++ foldl (\a b -> a ++ (if null a then "" else ", ") ++ show b) [] x ++ "]"
  show (SParenthesis x) = "Parenthesis: (" ++ foldl (\a b -> a ++ (if null a then "" else ", ") ++ show b) [] x ++ ")"
  show (SBracket x) = "Bracket: {" ++ foldl (\a b -> a ++ (if null a then "" else ", ") ++ show b) [] x ++ "}"
  show (SStruct x) = "Struct: {" ++ intercalate ", " (map showElement x) ++ "}" where showElement elems = "[" ++ intercalate ", " (map show elems) ++ "]"
  show (SLine x) = "Line: [" ++ foldl (\a b -> a ++ (if null a then "" else ", ") ++ show b) [] x ++ "]"

instance Eq SExpr where
  (SInt x1) == (SInt x2) = x1 == x2
  (SFloat f1) == (SFloat f2) = f1 == f2
  (SBool b1) == (SBool b2) = b1 == b2
  (SString str1) == (SString str2) = str1 == str2
  (SSymbol s1) == (SSymbol s2) = s1 == s2
  (SArray a1) == (SArray a2) = a1 == a2
  (SParenthesis p1) == (SParenthesis p2) = p1 == p2
  (SBracket br1) == (SBracket br2) = br1 == br2
  (SStruct st1) == (SStruct st2) = st1 == st2
  (SLine l1) == (SLine l2) = l1 == l2
  _ == _ = False


