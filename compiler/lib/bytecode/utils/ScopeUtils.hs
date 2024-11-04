{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- ScopeUtils
-}

module ScopeUtils (
  modifyArg,
  hasVarInScopes,
  getVar,
  hasArgInScopes,
  getArgIndexFromScopes,
  getArgIndexFromScope,
  createScope,
  addArgsToScope,
  addArgToScope,
  addVarsToScope,
  addVarToScope,
  clearScope
) where

import AstData
import Symbol (Symbol)
import BytecodeOperations
import BytecodeTypes

modifyArg :: String -> BParams -> BResult
modifyArg sym (_, (args, _), _, _) =
  case getArgIndexFromScopes sym args of
    -1 -> Left ("Invalid variable name: variable \"" ++ sym ++ "\" does not exist.")
    i -> Right (getModifyArg i)

hasVarInScopes :: String -> BParams -> Bool
hasVarInScopes var (_, _, vars, _) = checkVarInScopes var vars

getVar :: String -> BParams -> Maybe Ast
getVar var (_, _, vars, _) = getVarFromScopes var vars

getVarFromScopes :: String -> [[(Symbol, Ast)]] -> Maybe Ast
getVarFromScopes _ [] = Nothing
getVarFromScopes arg (x:xs) =
  case (getVarFromScope arg x) of
    Nothing -> getVarFromScopes arg xs
    result -> result

getVarFromScope :: String -> [(Symbol, Ast)] -> Maybe Ast
getVarFromScope _ [] = Nothing
getVarFromScope arg ((key, index):_) | arg == key = Just index
getVarFromScope arg (_:xs) = getVarFromScope arg xs

checkVarInScopes :: String -> [[(Symbol, Ast)]] -> Bool
checkVarInScopes _ [] = False
checkVarInScopes arg (x:xs) =
  if checkVarInScope arg x
  then True
  else checkVarInScopes arg xs

checkVarInScope :: String -> [(Symbol, Ast)] -> Bool
checkVarInScope _ [] = False
checkVarInScope arg ((key, _):_) | arg == key = True
checkVarInScope arg (_:xs) = checkVarInScope arg xs

hasArgInScopes :: String -> BParams -> Bool
hasArgInScopes arg (_, (args, _), _, _) = (getArgIndexFromScopes arg args) /= -1

getArgIndexFromScopes :: String -> [[(Symbol, Int)]] -> Int
getArgIndexFromScopes _ [] = -1
getArgIndexFromScopes arg (x:xs) =
  if r /= -1
  then r
  else getArgIndexFromScopes arg xs
    where
      r = getArgIndexFromScope arg x

getArgIndexFromScope :: String -> [(Symbol, Int)] -> Int
getArgIndexFromScope _ [] = -1
getArgIndexFromScope arg ((key, index):_) | arg == key = index
getArgIndexFromScope arg (_:xs) = getArgIndexFromScope arg xs

createScope :: String -> BParams -> BParams
createScope label (path, (args, index), vars, labels) = (label:path, ([]:args, index), []:vars, labels)

addArgsToScope :: [String] -> BParams -> (BParams, String)
addArgsToScope [] params = (params, "")
addArgsToScope (x:xs) params = (rParams, r1 ++ r2)
  where
    (nParams, r1) = addArgToScope x params
    (rParams, r2) = addArgsToScope xs nParams

addArgToScope :: String -> BParams -> (BParams, String)
addArgToScope arg (path, ((scope:args), index), vars, labels) = ((path, (((arg, index):scope):args, index + 1), vars, labels), getPushStackOnArg)
addArgToScope _ p = (p, "")

addVarsToScope :: [(Symbol, Ast)] -> BParams -> BParams
addVarsToScope [] params = params
addVarsToScope (x:xs) params = addVarsToScope xs nParams
  where
    nParams = addVarToScope x params

addVarToScope :: (Symbol, Ast) -> BParams -> BParams
addVarToScope x (path, args, (scope:vars), labels) = (path, args, ((x:scope):vars), labels)
addVarToScope _ p = p

clearScope :: BParams -> (BParams, String)
clearScope ((_:path), args, vars, labels) = ((path, nArgs, nVars, labels), result)
  where
    (nArgs, result) = clearArgsScope args
    nVars = clearVarsScope vars
clearScope p = (p, "")

clearArgsScope :: BArgs -> (BArgs, String)
clearArgsScope ([], _) = (([], 0), "")
clearArgsScope (x:xs, index) = ((xs, nIndex), res)
  where
    (res, nIndex) = clearArgs x index

clearArgs :: [(Symbol, Int)] -> Int -> (String, Int)
clearArgs [] i = ("", i)
clearArgs (_:xs) i = (getPopArg ++ result, nIndex - 1)
  where
    (result, nIndex) = clearArgs xs i

clearVarsScope :: BVars -> BVars
clearVarsScope [] = []
clearVarsScope (_:xs) = xs
