{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- AstData
-}

module AstData (Ast(..), AstDeclaration(..), AstAssignation(..), AstCall(..), AstLoop(..), AstObjectElement(..)) where

import Symbol

data AstDeclaration = FuncDeclaration { declareArgs :: [Symbol], declareBody :: Ast } deriving (Eq)

data AstAssignation =
  VarAssignation { assignationKey :: Symbol, assignationValue :: Ast } |
  AccessAssignation { assignationAccessArray :: Ast, assignationAccessArg :: Ast, assignationAccessValue :: Ast }
  deriving (Eq)

data AstCall =
  FuncCall { callFunction :: Ast, callArgs :: [Ast] } |
  ArrayAccess { accessArray :: Ast, accessArg :: Ast }
  deriving (Eq)

data AstLoop =
  ForLoop { forAssignation :: [Ast], forCondition :: Ast, forIncrementation :: [Ast], forBody :: Ast } |
  WhileLoop { whileCondition :: Ast, whileBody :: Ast }
  deriving (Eq)

data AstObjectElement = ObjectElement { objectKey :: Symbol, objectValue :: Ast} deriving (Eq)

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
  (AAssignation (AccessAssignation x y z)) == (AAssignation (AccessAssignation x' y' z')) = x == x' && y == y' && z == z'
  (ADeclaration (FuncDeclaration x y)) == (ADeclaration (FuncDeclaration x' y')) = x == x' && y == y'
  (ACall (FuncCall x y)) == (ACall (FuncCall x' y')) = x == x' && y == y'
  (ACall (ArrayAccess x y)) == (ACall (ArrayAccess x' y')) = x == x' && y == y'
  (ALoop (ForLoop x y z w)) == (ALoop (ForLoop x' y' z' w')) = x == x' && y == y' && z == z' && w == w'
  (ALoop (WhileLoop x y)) == (ALoop (WhileLoop x' y')) = x == x' && y == y'
  (AObject x) == (AObject y) = x == y
  _ == _ = True

instance Show Ast where
  show AUndefined = "undefined"
  show (AInt x) = show x
  show (AFloat x) = show x
  show (ABool x) = if x then "true" else "false"
  show (ASymbol x) = x
  show (AString x) = "\"" ++ x ++ "\""
  show (AList x) = "[ " ++ foldl (\a b -> a ++ (if null a then "" else ", ") ++ show b) [] x ++ " ]"
  show (AObject x) = "{ " ++ foldl (\a b -> a ++ (if null a then "" else ", ") ++ show b) [] x ++ " }"
  show (ALine _) = "#\\<code block\\>"
  show (ADeclaration x) = "#\\<declaration\\>" ++ show x
  show (AAssignation x) = "#\\<assignation\\>" ++ show x
  show (ACall x) = "#\\<call\\>" ++ show x
  show (ALoop x) = "#\\<loop\\>" ++ show x


instance Show AstDeclaration where
  show (FuncDeclaration {}) = " [type=function]"

instance Show AstAssignation where
  show (VarAssignation { assignationKey = key }) = " [type=variable]: assign '" ++ key ++ "'"
  show (AccessAssignation { assignationAccessArray = arr, assignationAccessArg = arg }) = " [type=access]: assign '" ++ show arr ++ "[" ++ show arg ++ "]'"

instance Show AstCall where
  show (FuncCall {}) = " [type=function]"
  show (ArrayAccess {accessArray = arr, accessArg = arg}) = " [type=array]: " ++ show arr ++ "[" ++ show arg ++ "]"

instance Show AstLoop where
  show (ForLoop {}) = " [type=for]"
  show (WhileLoop {}) = " [type=while]"

instance Show AstObjectElement where
  show (ObjectElement {objectKey = key, objectValue = value}) = key ++ ": " ++ show value
