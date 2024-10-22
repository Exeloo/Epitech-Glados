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
   SFloat Float |
   SBool Bool |
   SString String |
   SSymbol Symbol |
   SArray [SExpr] |
   SStruct [(Symbol, SExpr)] |
   SList [SExpr] |
   Undefined

instance Show SExpr where
  show (SInt x) = "Integer: " ++ show x
  show (SFloat x) = "Float: " ++ show x
  show (SBool x) = "Boolean: " ++ show x
  show (SString x) = "String: \"" ++ x ++ "\""
  show (SSymbol x) = "Symbol: " ++ show x
  show (SArray x) = "Array: [" ++ foldl (\a b -> a ++ (if null a then "" else ", ") ++ show b) [] x ++ "]"
  show (SStruct x) = "Struct: {" ++ foldl (\a (b, c) -> a ++ (if null a then "" else ", ") ++ show b ++ ": " ++ show c) [] x ++ "}"
  show (SList x) = "List: [" ++ foldl (\a b -> a ++ (if null a then "" else ", ") ++ show b) [] x ++ "]"
  show Undefined = "Undefined"

instance Eq SExpr where
  (SInt x1) == (SInt x2) = x1 == x2
  (SFloat f1) == (SFloat f2) = f1 == f2
  (SBool b1) == (SBool b2) = b1 == b2
  (SString str1) == (SString str2) = str1 == str2
  (SSymbol s1) == (SSymbol s2) = s1 == s2
  (SArray a1) == (SArray a2) = a1 == a2
  (SStruct struct1) == (SStruct struct2) = struct1 == struct2
  (SList l1) == (SList l2) = l1 == l2
  Undefined == Undefined = True
  _ == _ = False


