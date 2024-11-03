{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- ScopeUtils
-}

module ScopeUtils (
  hasVarInScopes,
  getVar,
  hasArgInScopes,
  getArgIndexFromScopes,
  getArgIndexFromScope,
  createScopeWithArgs,
  createScope,
  addArgsToScope,
  addArgToScope,
  addVarsToScope,
  addVarToScope,
  clearScope
) where

import AstData
import BytcodeOperations
import BytecodeTypes

hasVarInScopes :: String -> BParams -> Bool
hasVarInScopes var (_, _, vars, _) = checkVarInScopes var vars

getVar :: String -> BParams -> Ast
getVar var (_, _, vars, _) = getVarFromScopes var vars

getVarFromScopes :: String -> [[(Symbol, Ast)]] -> Ast
getVarFromScopes _ [] = AInt 0
getVarFromScopes arg (x:xs) =
  if r != -1
  then r
  else getVarFromScopes arg xs
    where
      r = getVarFromScope arg x

getVarFromScope :: String -> [(Symbol, Ast)] -> Ast
getVarFromScope _ [] = AInt 0
getVarFromScope arg ((key, index):_) | arg == key = index
getVarFromScope arg (x:xs) = getArgIndexFromScope arg xs

checkVarInScopes :: String -> [[(Symbol, Ast)]] -> Bool
checkVarInScopes _ [] = False
checkVarInScopes arg (x:xs) =
  if r
  then r
  else getArgIndexFromScopes arg xs
    where
      r = getArgIndexFromScope arg x

checkVarInScope :: String -> [(Symbol, Ast)] -> Bool
checkVarInScope _ [] = False
checkVarInScope arg ((key, index):_) | arg == key = True
checkVarInScope arg (x:xs) = getArgIndexFromScope arg xs

hasArgInScopes :: String -> BParams -> Bool
hasArgInScopes arg (_, { bArgs = args }, _, _) = (getArgIndexFromScopes arg args) != -1

getArgIndexFromScopes :: String -> [[(Symbol, Int)]] -> Int
getArgIndexFromScopes _ [] = -1
getArgIndexFromScopes arg (x:xs) =
  if r != -1
  then r
  else getArgIndexFromScopes arg xs
    where
      r = getArgIndexFromScope arg x

getArgIndexFromScope :: String -> [(Symbol, Int)] -> Int
getArgIndexFromScope _ [] = -1
getArgIndexFromScope arg ((key, index):_) | arg == key = index
getArgIndexFromScope arg (x:xs) = getArgIndexFromScope arg xs

createScopeWithArgs :: [String] -> BParams -> (BParams, String)
createScopeWithArgs args p = addArgsToScope args nParams
  where
    nParams = createScope p

createScope :: BParams -> BParams
createScope (path, { bArgs = args, bIndex = index }, vars, labels) = (path, { bArgs = []:args, bIndex = index }, []:vars, labels)

addArgsToScope :: [String] -> BParams -> (BParams, String)
addArgsToScope [] params = (params, "")
addArgsToScope (x:xs) params = (rParams, r1 ++ r2)
  where
    (nParams, r1) = addVarToScope x params
    (rParams, r2) = addVarsToScope xs nParams

addArgToScope :: String -> BParams -> (BParams, String)
addArgToScope arg (path, { bArgs = (scope:args), bIndex = index }, vars, labels) = ((path, { bArgs = ((arg, index):scope):args, bIndex = index + 1 }, vars, labels), getPushStackOnArg)

addVarsToScope :: [(Symbol, Ast)] -> BParams -> BParams
addVarsToScope [] params = params
addVarsToScope (x:xs) params = addVarsToScope xs nParams
  where
    nParams = addVarToScope x params

addVarToScope :: (Symbol, Ast) -> BParams -> BParams
addVarToScope x (path, args, (scope:vars), labels) = (path, args, ((x:scope):vars), labels)

clearScope :: BParams -> (BParams, String)
clearScope (path, args, vars, labels) = ((path, nArgs, nVars, labels), result)
  where
    (nArgs, result) = clearArgsScope args
    nVars = clearVarsScope vars

clearArgsScope :: BArgs -> (BArgs, String)
clearArgsScope { bArgs = [] } = ("", { bArgs = [], bIndex = 0 })
clearArgsScope { bArgs = (x:xs), bIndex = index } = (xs, clearVars x)

clearArgs :: [(Symbol, Int)] -> Int -> (String, Int)
clearArgs [] i = ("", i)
clearArgs (_:xs) i = (getPopArg ++ result, nIndex - 1)
  where
    (result, nIndex) = clearArgs xs i

clearVarsScope :: BVars -> BVars
clearVarsScope [] = []
clearVarsScope (_:xs) = xs
