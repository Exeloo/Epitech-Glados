{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- AstData
-}

module AstData (Ast(..), AstDeclaration(..), AstAssignation(..), AstCall(..), AstLoop(..), AstObjectElement(..)) where

import Symbol

data AstDeclaration = FuncDeclaration { declareArgs :: [Symbol], declareBody :: Ast } deriving (Show, Eq)

data AstAssignation =
  VarAssignation { assignationKey :: Symbol, assignationValue :: Ast } |
  AccessAssignation { assignationAccessArray :: Ast, assignationAccessArg :: Ast, assignationAccessValue :: Ast }
  deriving (Show, Eq)

data AstCall =
  FuncCall { callFunction :: Ast, callArgs :: [Ast] } |
  ArrayAccess { accessArray :: Ast, accessArg :: Ast }
  deriving (Show, Eq)

data AstLoop =
  ForLoop { forAssignation :: [Ast], forCondition :: Ast, forIncrementation :: [Ast], forBody :: Ast } |
  WhileLoop { whileCondition :: Ast, whileBody :: Ast }
  deriving (Show, Eq)

data AstObjectElement = ObjectElement { objectKey :: Symbol, objectValue :: Ast} deriving (Show, Eq)

data Ast =
  AUndefined |
  ASymbol Symbol |
  ABool Bool |
  AInt Int |
  AFloat Float |
  AString String |
  ALine [Ast] |
  AList [Ast] |
  AObject [AstObjectElement] |
  ADeclaration AstDeclaration |
  AAssignation AstAssignation |
  ACall AstCall |
  ALoop AstLoop

instance Eq Ast where
  (AInt x) == (AInt y) = x == y
  (AFloat x) == (AFloat y) = x == y
  (ABool x) == (ABool y) = x == y
  (ASymbol x) == (ASymbol y) = x == y
  (AString x) == (AString y) = x == y
  (AList xs) == (AList ys) = xs == ys
  (AAssignation (VarAssignation x y)) == (AAssignation (VarAssignation x' y')) = x == x' && y == y'
  (ADeclaration (FuncDeclaration x y)) == (ADeclaration (FuncDeclaration x' y')) = x == x' && y == y'
  (ACall (FuncCall x y)) == (ACall (FuncCall x' y')) = x == x' && y == y'
  (ACall (ArrayAccess x y)) == (ACall (ArrayAccess x' y')) = x == x' && y == y'
  (ALoop (ForLoop x y z w)) == (ALoop (ForLoop x' y' z' w')) = x == x' && y == y' && z == z' && w == w'
  (ALoop (WhileLoop x y)) == (ALoop (WhileLoop x' y')) = x == x' && y == y'
  (AObject x) == (AObject y) = x == y
  _ == _ = False

instance Show Ast where
  show (AInt x) = show x
  show (ABool x) = if x then "#t" else "#f"
  show (ASymbol x) = x
  show (AString x) = x
  show (AList x) = "[" ++ foldl (\a b -> a ++ (if null a then "" else ", ") ++ show b) [] x ++ "]"
  show _ = "#\\<procedure\\>"
