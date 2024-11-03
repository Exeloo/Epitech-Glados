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

modifyArg :: String -> (BParams -> BResult) -> BParams -> PBResult
modifyArg sym cb (path, (args, index), vars, labels) = ((path, (args, index), vars, labels), modifyArgInScopes sym cb args index (path, (args, index), vars, labels))

modifyArgInScopes :: String -> (BParams -> BResult) -> [[(Symbol, Int)]] -> Int -> BParams -> BResult
modifyArgInScopes sym _ [] _ _ = Left ("Invalid argument: variable " ++ sym ++ " does not exist. Please check if it is available in the current scope.")
modifyArgInScopes sym cb (arg:args) index (path, eArgs, vars, labels) =
  if found
  then getInstructionsForModify left right cb (path, (tArgs:args, nIndex), vars, labels)
  else modifyArgInScopes sym cb args nIndex (path, eArgs, vars, labels)
    where
      (found, left, right, tArgs, nIndex) = modifyArgInScope sym arg index

getInstructionsForModify :: String -> String -> (BParams -> BResult) -> BParams -> BResult
getInstructionsForModify left right cb p = (++) <$> ((++) <$> Right left <*> (cb p)) <*> Right right

modifyArgInScope :: String -> [(Symbol, Int)] -> Int -> (Bool, String, String, [(Symbol, Int)], Int)
modifyArgInScope _ [] _ = (False, "", "", [], 0)
modifyArgInScope name ((sym, index):args) i | name == sym = (True, "", getPushStackOnArg, (sym, index):args, i)
modifyArgInScope name ((_, index):xs) i = (found, getPushArgOnStack index ++ getPopArg ++ left, right ++ getPushStackOnArg, nArgs, nIndex)
  where
      (found, left, right, nArgs, nIndex) = modifyArgInScope name xs (i - 1)

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
