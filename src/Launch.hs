{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- Launch
-}

module Launch (launch, fileExist, getParamsLine) where

import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)
import System.IO.Error (isEOFError)
import System.IO (hFlush, stdout, hIsTerminalDevice, stdin, isEOF)
import Data.Maybe (isJust, fromJust)
import Text.Megaparsec (parse, errorBundlePretty)

import Parser

handleCtrl :: IOException -> IO Bool
handleCtrl e | isEOFError e = return True
             | otherwise = return True

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
            then getInput flag
        else
            return False

getInput :: Bool -> IO Bool
getInput flag =
    hIsTerminalDevice stdin >>= \isTerminal ->
    if not isTerminal
        then launchFileInput flag
    else
        (putStr "> " >>
        hFlush stdout >>
        getInputLine flag >>
        getInput flag) `catch` handleCtrl


checkParse :: String -> Bool -> IO Bool
checkParse line flag =
    case parse parseSList "" line of
        Left err -> putStrLn ("Parse error: " ++ errorBundlePretty err) >> return False
        Right res -> putStrLn (line ++ if flag then " flag" else "") >> return True -- compile with flag "-l"

checkLines :: [String] -> Bool -> IO Bool
checkLines [] _ = return True
checkLines (line:rest) flag =
    checkParse line flag >>= \res ->
    if res
        then checkLines rest flag
    else
        return False

launchParams :: [String] -> IO Bool
launchParams args =
    fileExist args >>= \file ->
    if (isJust file)
        then getFile ("-l" `elem` args) (fromJust file)
    else
        simpleLine ("-l" `elem` args) (getParamsLine args)


getInputLine :: Bool -> IO Bool
getInputLine flag =
    getLine >>= \line ->
    checkParse line flag

getFile :: Bool -> String -> IO Bool
getFile flag path =
    readFile path >>= \content ->
    checkLines (lines content) flag

simpleLine :: Bool -> String -> IO Bool
simpleLine flag str = checkParse str flag

launch :: [String] -> IO Bool
launch [] = getInput False
launch args | (length args > 2) = return False
            | (length args == 2) && not ("-l" `elem` args) = return False
            | ("-l" `elem` args) && (length args == 1) = getInput True
            | otherwise = launchParams args
