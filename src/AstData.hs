{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- AstData
-}

module AstData (Ast(..), AstDeclaration(..), AstAssignation(..), AstFuncArg(..), AstCall(..)) where

import Symbol

data AstDeclaration = FuncDeclaration { declareArgs :: [Symbol], declareBody :: [Ast] } deriving Show

data AstAssignation = VarAssignation { assignationKey :: Symbol, assignationValue :: Ast } deriving Show

data AstFuncArg = FFunc AstDeclaration | FSymbol Symbol deriving Show

data AstCall = FuncCall { callFunction :: AstFuncArg, callArgs :: [Ast] } deriving Show

data Ast =
  ASymbol Symbol |
  ABool Bool |
  AInt Int |
  AString String |
  AList [Ast] |
  ADeclaration AstDeclaration |
  AAssignation AstAssignation |
  ACall AstCall
    deriving Show
