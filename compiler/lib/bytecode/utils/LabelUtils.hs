{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- LabelUtils
-}

module LabelUtils (
  isInFunction,
  findFunctionLabel,
  goToFunctionInLabel,
  goToFunctionOutLabel,
  goToLoopInLabel,
  goToLoopOutLabel,
  getBuiltinLabel,
  getFunctionLabel,
  getIfLabel,
  getForLabel,
  getWhileLabel,
  getLabelWithIO
) where

import Data.List (isPrefixOf)
import Data.Char (toLower)

import Symbol (Symbol)

import BytecodeOperations
import BytecodeTypes

-- Resolve label in current path

goToFunctionInLabel :: BParams -> (String, PBResult)
goToFunctionInLabel (path, args, vars, labels) = case findFunctionLabel path of
  Nothing -> ("", ((path, args, vars, labels), Left "Invalid call: Not in a function but try to recursive it"))
  Just label -> (label, ((path, args, vars, labels), Right (getJump label)))

goToFunctionOutLabel :: BParams -> PBResult
goToFunctionOutLabel (path, args, vars, labels) = ((nPath, args, vars, labels), getJump <$> label)
  where
    (nPath, label) = resolveFunctionLabel path 1

isInFunction :: String -> BPath -> Bool
isInFunction _ [] = False
isInFunction name (x:xs) =
  if isRightLabel "function" x
  then isRightLabel ("function_" ++ (map toLower name)) x
  else isInFunction name xs

findFunctionLabel :: BPath -> Maybe String
findFunctionLabel [] = Nothing
findFunctionLabel (x:xs) =
  if isRightLabel "function" x
  then Just x
  else findFunctionLabel xs

resolveFunctionLabel :: BPath -> Int -> (BPath, Either String String)
resolveFunctionLabel [] _ = ([], Left "Invalid label: not inside a function")
resolveFunctionLabel (x:xs) io =
  if isRightLabel x "function"
  then (xs, Right (getLabelWithIO x io))
  else resolveFunctionLabel xs io

goToLoopInLabel :: BParams -> PBResult
goToLoopInLabel (path, args, vars, labels) = ((path, args, vars, labels), getJump <$> label)
  where
    (_, label) = resolveLoopLabel path 0

goToLoopOutLabel :: BParams -> PBResult
goToLoopOutLabel (path, args, vars, labels) = ((nPath, args, vars, labels), getJump <$> label)
  where
    (nPath, label) = resolveLoopLabel path 1

resolveLoopLabel :: BPath -> Int -> (BPath, Either String String)
resolveLoopLabel [] _ = ([], Left "Invalid label: not inside a loop")
resolveLoopLabel (x:xs) io =
  if isRightLabel x "function"
  then ([], Left "Invalid label: not inside a loop")
  else
    if isRightLabel x "for" || isRightLabel x "while"
    then (xs, Right (getLabelWithIO x io))
    else resolveLoopLabel xs io

isRightLabel :: String -> String -> Bool
isRightLabel label str = isPrefixOf (label ++ "_") str

-- Add new label

getBuiltinLabel :: String -> BLabels -> (String, BLabels)
getBuiltinLabel name labels = createLabel ("builtin_" ++ (map toLower name)) labels

getFunctionLabel :: String -> BLabels -> (String, BLabels)
getFunctionLabel name labels = createLabel ("function_" ++ (map toLower name)) labels

getIfLabel :: BLabels -> (String, BLabels)
getIfLabel labels = createLabel "if" labels

getForLabel :: BLabels -> (String, BLabels)
getForLabel labels = createLabel "for" labels

getWhileLabel :: BLabels -> (String, BLabels)
getWhileLabel labels = createLabel "while" labels

createLabel :: String -> BLabels -> (String, BLabels)
createLabel s [] = createLabel s [(s, 0)]
createLabel s ((key, nb):xs) | s == key = (key ++ "_" ++ show nb, (key, nb + 1):xs)
createLabel s (x:xs) = (label, x:rest)
  where
    (label, rest) = createLabel s xs

getLabelWithIO :: String -> Int -> String
getLabelWithIO str io = str ++ "_" ++ (getIOLabel io)

getIOLabel :: Int -> String
getIOLabel 0 = "in"
getIOLabel 1 = "out"
getIOLabel _ = "undefined"
