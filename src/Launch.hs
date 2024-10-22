{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- Launch
-}

module Launch (launch, fileExist, handleCtrl) where

import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)
import System.IO.Error (isEOFError)
import System.IO (hFlush, stdout, hIsTerminalDevice, stdin, isEOF)
import Data.Maybe (isJust, fromJust)
import Text.Megaparsec (parse, errorBundlePretty)

import Parser
import SExprData
import SExprToAst

handleCtrl :: IOException -> Bool -> IO Bool
handleCtrl e res | isEOFError e = return res
                 | otherwise = return res

fileExist :: String -> IO (Maybe String)
fileExist path =
    doesFileExist path >>= \exist ->
    if exist
        then return (Just path)
    else
        return Nothing


launchFileInput :: IO Bool
launchFileInput =
    isEOF >>= \isEnd ->
    if isEnd
        then return True
    else
        getInputLine >>= \res ->
        if res
            then launchFileInput
        else
            return False

getInput :: Bool -> IO Bool
getInput res =
    hIsTerminalDevice stdin >>= \isTerminal ->
    if not isTerminal
        then launchFileInput
    else
        (putStr "> " >>
        hFlush stdout >>
        getInputLine >>= \resLine ->
        getInput resLine) `catch` (\e -> handleCtrl e res)


checkParse :: String -> IO Bool
checkParse line =
    case parse parseSList "" line of
        Left err -> putStrLn ("Parse error: " ++ errorBundlePretty err) >> return False
        Right res -> getAst res

checkLines :: [String] -> IO Bool
checkLines [] = return True
checkLines (line:rest) =
    checkParse line >>= \res ->
    if res
        then checkLines rest
    else
        return False

launchParams :: String -> IO Bool
launchParams arg =
    fileExist arg >>= \file ->
    if (isJust file)
        then getFile (fromJust file)
    else
        simpleLine arg

getInputLine :: IO Bool
getInputLine =
    getLine >>= \line ->
    checkParse line

getFile :: String -> IO Bool
getFile path =
    readFile path >>= \content ->
    checkLines (lines content)

simpleLine :: String -> IO Bool
simpleLine str = checkParse str

launch :: [String] -> IO Bool
launch [] = getInput True
launch (x:xs) | (length (x:xs) > 1) = return False
              | otherwise = launchParams x

getAst :: SExpr -> IO Bool
getAst x = case sExpToAst x of
    Left a -> putStrLn (a) >> return False
    Right a -> putStrLn (show a) >> return True
