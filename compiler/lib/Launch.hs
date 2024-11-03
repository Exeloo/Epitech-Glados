{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- Launch
-}

module Launch (launch, errorHandling, getFilePath, handleCtrl, fileExist, Info) where

import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)
import System.IO.Error (isEOFError)
import System.IO (hFlush, stdout, hIsTerminalDevice, stdin, isEOF)
import Data.Maybe (fromJust)
import Parser
import SExprToAst
import SExprData
import AstData
import AstToBytecode
import Text.Megaparsec (parse, errorBundlePretty)

type Info = ((Bool, Bool), String)

handleCtrl :: IOException -> String -> Bool -> IO Bool
handleCtrl e res compile | isEOFError e = checkParse res compile
                         | otherwise = checkParse res compile


getFilePath :: [String] -> Maybe String
getFilePath args =
    let notFlag = filter (\arg -> not (arg `elem` ["-f", "-c"])) args
    in if null notFlag
        then Nothing
       else Just (last notFlag)

fileExist :: String -> IO (Maybe String)
fileExist path =
    doesFileExist path >>= \exist ->
    if exist
        then return (Just path)
    else return Nothing


checkFile :: [String] -> IO (Maybe Info)
checkFile args =
    case getFilePath args of
        Just path -> fileExist path >>= \fileExists ->
            if fileExists /= Nothing
                then return $ Just ((True, "-c" `elem` args), path)
            else putStrLn "File doesn't exist, please refer to the --help" >> return Nothing
        Nothing -> return $ Just ((False, "-c" `elem` args), "")

errorHandling :: [String] -> IO (Maybe Info)
errorHandling args | length args > 3 = putStrLn "Too many arguments, please refer to the --help" >> return Nothing
                   | length args == 3 && not ("-f" `elem` args && "-c" `elem` args) = putStrLn "Incorrect arguments, please refer to the --help" >> return Nothing
                   | length args == 2 && "-c" `elem` args = putStrLn "Incorrect use, please refer to the --help" >> return Nothing
                   | length args == 1 && "-f" `elem` args = putStrLn "Forgot file, please refer to the --help" >> return Nothing
                   | length args == 1 && not ("-c" `elem` args) = putStrLn "Incorrect use, please refer to the --help" >> return Nothing
                   | otherwise = checkFile args

compileAst :: Ast -> IO Bool
compileAst resAst = case astToBytecode resAst of
    Left err -> putStrLn err >> return False
    Right res -> putStrLn res >> return True

getAst :: SExpr -> Bool -> IO Bool
getAst sExp _ = case sExpToAst sExp of
    Left err -> putStrLn err >> return False
    Right res -> compileAst res

checkParse :: String -> Bool -> IO Bool
checkParse content compile =
    case parse parseSBlock "" content of
        Left err -> putStrLn ("Parse error: " ++ errorBundlePretty err) >> return False
        Right res -> getAst res compile

launchFileInput :: Bool -> String -> IO Bool
launchFileInput compile finalInput =
    isEOF >>= \isEnd ->
    if isEnd
        then checkParse (init finalInput) compile
    else
        getLine >>= launchFileInput compile . (finalInput ++) . (++ "\n")

promptInput :: Bool -> String -> IO Bool
promptInput compile res =
    (putStr "> " >>
    hFlush stdout >>
    getLine >>= promptInput compile . (res ++) . (++ "\n")) `catch` (\e -> handleCtrl e res compile)

getInput :: Bool -> IO Bool
getInput compile =
    hIsTerminalDevice stdin >>= \isTerminal ->
    if not isTerminal
        then launchFileInput compile ""
    else
        promptInput compile ""

launchFile :: String -> Bool -> IO Bool
launchFile file compile =
    readFile file >>= \content ->
    checkParse content compile

chooseMode :: Info -> IO Bool
chooseMode ((False, compile), _) = getInput compile
chooseMode ((True, compile), file) = launchFile file compile


launch :: [String] -> IO Bool
launch args =
    errorHandling args >>= \infos ->
    if infos == Nothing
        then return False
    else (chooseMode (fromJust infos))