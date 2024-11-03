{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- LabelUtils
-}

module LabelUtils (
  goToFunctionInLabel,
  goToFunctionOutLabel,
  goToLoopInLabel,
  goToLoopOutLabel,
  getBuiltinLabel,
  getFunctionLabel,
  getIfLabel,
  getForLabel,
  getWhileLabel
) where

import Data.List (isPrefixOf)
import Data.Text (toLower)

import BytcodeOperations
import BytecodeTypes

-- Resolve label in current path

goToFunctionInLabel :: BytecodeParams -> PBResult
goToFunctionInLabel (path, args, vars, labels) -> ((path, args, vars, labels), getJump result)
  where
    (_, label) = resolveFunctionLabel path 0

goToFunctionOutLabel :: BytecodeParams -> PBResult
goToFunctionOutLabel (path, args, vars, labels) -> ((nPath, args, vars, labels), getJump result)
  where
    (nPath, label) = resolveFunctionLabel path 1

resolveFunctionLabel :: BPath -> Int -> (BPath, Either String String)
resolveFunctionLabel [] _ = Left "Invalid label: not inside a function"
resolveFunctionLabel (x:xs) io =
  if isRightLabel "function"
  then (xs, Right getLabelWithIO x io)
  else resolveFunctionLabel xs io

goToLoopInLabel :: BytecodeParams -> PBResult
goToLoopInLabel (path, args, vars, labels) -> ((path, args, vars, labels), getJump result)
  where
    (_, label) = resolveLoopLabel path 0

goToLoopOutLabel :: BytecodeParams -> PBResult
goToLoopOutLabel (path, args, vars, labels) -> ((nPath, args, vars, labels), getJump result)
  where
    (nPath, label) = resolveLoopLabel path 1

resolveLoopLabel :: BPath -> Int -> (BPath, Either String String)
resolveLoopLabel [] _ = Left "Invalid label: not inside a loop"
resolveLoopLabel (x:xs) io =
  if isRightLabel "function"
  then ([], Left "Invalid label: not inside a loop")
  else
    if isRightLabel "for" || isRightLabel "while"
    then (xs, Right getLabelWithIO x io)
    else resolveLoopLabel xs io

isRightLabel :: String -> String -> Bool
isRightLabel label str = isPrefixOf (label ++ "_") str

-- Add new label

getBuiltinLabel :: String -> BLabels -> (String, BLabels)
getBuiltinLabel name labels = resolveLabel ("builtin_" ++ (toLower name)) labels

getFunctionLabel :: String -> BLabels -> (String, BLabels)
getFunctionLabel name labels = resolveLabel ("function_" ++ (toLower name)) labels

getIfLabel :: BLabels -> (String, BLabels)
getIfLabel labels = resolveLabel "if" labels

getForLabel :: BLabels -> (String, BLabels)
getForLabel labels = resolveLabel "for" labels

getWhileLabel :: BLabels -> (String, BLabels)
getWhileLabel labels = resolveLabel "while" labels

resolveLabel :: String -> BLabels -> (String, BLabels)
resolveLabel s [] = resolveLabel s [(s, 0)]
resolveLabel s [(key, nb):xs] | s == key = (key ++ "_" ++ show nb, (key, nb + 1):xs)
resolveLabel s _ [x:xs] = (label, x:rest)
  where
    (label, rest) = resolveLabel s xs

getLabelWithIO :: String -> Int -> String
getLabelWithIO str io = str ++ "_" ++ (getIOLabel io)

getIOLabel :: Int -> String
getIOLabel 0 = "in"
getIOLabel 1 = "out"
getIOLabel _ = "undefined"


