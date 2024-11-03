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
import Text.Megaparsec (parse, errorBundlePretty)

type Info = ((Bool, Bool), String)

handleCtrl :: IOException -> Bool -> IO Bool
handleCtrl e res | isEOFError e = return res
                 | otherwise = return res


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

-- evaluateAst :: Ast -> IO Bool
-- evaluateAst resAst = case evalAst [[]] resAst of
--     Left error -> putStrLn (error) >> return False
--     Right res -> putStrLn (res) >> return True

-- compileAst :: Ast -> IO Bool
-- compileAst resAst = case ByteAst resAst of
--     Left error -> putStrLn (error) >> return False
--     Right res -> putStrLn (res) >> return True

-- getAst :: SExpr -> Bool -> IO Bool
-- getAst sExp compile = case sExpToAst sExp of
--     Left error -> putStrLn (error) >> return False
--     Right res -> if compile
--                      then compileAst res
--                  else evaluateAst res

checkParse :: String -> Bool -> IO Bool
checkParse content compile =
    case parse parseSBlock "" content of
        Left err -> putStrLn ("Parse error: " ++ errorBundlePretty err) >> return False
        Right res -> putStrLn (show res) >> return True

getInputLine :: Bool -> IO Bool
getInputLine compile =
    getLine >>= \line ->
    checkParse line compile

launchFileInput :: Bool -> String -> IO Bool
launchFileInput compile finalInput =
    isEOF >>= \isEnd ->
    if isEnd
        then if compile
                then checkParse finalInput compile
             else return True
        else if compile
                then getLine >>= launchFileInput compile . (finalInput ++) . (++ "\n")
             else getInputLine compile >>= \res ->
                if res
                    then launchFileInput compile ""
                else return False

promptInput :: Bool -> Bool -> IO Bool
promptInput res compile =
    (putStr "> " >>
    hFlush stdout >>
    getInputLine compile >>= \resLine ->
    getInput resLine compile) `catch` (\e -> handleCtrl e res)

getInput :: Bool -> Bool -> IO Bool
getInput res compile =
    hIsTerminalDevice stdin >>= \isTerminal ->
    if not isTerminal
        then launchFileInput compile ""
    else if compile
            then putStrLn "We cannot compile with the interactive shell, please refer to the --help" >> return False
         else promptInput res compile

launchFile :: String -> Bool -> IO Bool
launchFile file compile =
    readFile file >>= \content ->
    checkParse content compile

chooseMode :: Info -> IO Bool
chooseMode ((False, compile), _) = getInput True compile
chooseMode ((True, compile), file) = launchFile file compile


launch :: [String] -> IO Bool
launch args =
    errorHandling args >>= \infos ->
    if infos == Nothing
        then return False
    else (chooseMode (fromJust infos))