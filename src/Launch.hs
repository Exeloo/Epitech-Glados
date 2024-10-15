{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- Launch
-}

module Launch (launch, fileExist, getParamsLine, handleCtrl) where

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

fileExist :: [String] -> IO (Maybe String)
fileExist [] = return Nothing
fileExist (x:xs) =
    doesFileExist x >>= \exist ->
    if exist
        then return (Just x)
    else
        fileExist xs

getParamsLine :: [String] -> String
getParamsLine ("-l":[]) = []
getParamsLine ("-l":xs) = last xs
getParamsLine (x:_) = x
getParamsLine _ = []


launchFileInput :: Bool -> IO Bool
launchFileInput flag =
    isEOF >>= \isEnd ->
    if isEnd
        then return True
    else
        getInputLine flag >>= \res ->
        if res
            then launchFileInput flag
        else
            return False

getInput :: Bool -> Bool -> IO Bool
getInput flag res =
    hIsTerminalDevice stdin >>= \isTerminal ->
    if not isTerminal
        then launchFileInput flag
    else
        (putStr "> " >>
        hFlush stdout >>
        getInputLine flag >>= \resLine ->
        getInput flag resLine) `catch` (\e -> handleCtrl e res)


checkParse :: String -> Bool -> IO Bool
checkParse line flag =
    case parse parseSList "" line of
        Left err -> putStrLn ("Parse error: " ++ errorBundlePretty err) >> return False
        Right res -> getAst res -- compile with flag "-l"

checkLines :: [String] -> IO Bool
checkLines [] = return True
checkLines (line:rest) =
    checkParse line False >>= \res ->
    if res
        then checkLines rest
    else
        return False

launchParams :: [String] -> IO Bool
launchParams args =
    fileExist args >>= \file ->
    if (isJust file)
        then getFile (fromJust file)
    else
        simpleLine (getParamsLine args)

getInputLine :: Bool -> IO Bool
getInputLine flag =
    getLine >>= \line ->
    checkParse line flag

getFile :: String -> IO Bool
getFile path =
    readFile path >>= \content ->
    checkLines (lines content)

simpleLine :: String -> IO Bool
simpleLine str = checkParse str False

launch :: [String] -> IO Bool
launch [] = getInput False True
launch args | (length args > 2) = return False
            | (length args == 2) && not ("-l" `elem` args) = return False
            | ("-l" `elem` args) && (length args == 1) = getInput True True
            | otherwise = launchParams args

getAst :: SExpr -> IO Bool
getAst x = case sExpToAst x of
    Left a -> putStrLn (a) >> return False
    Right a -> putStrLn (show a) >> return True
