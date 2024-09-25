{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- Launch
-}

module Launch (launch, fileExist, getParamsLine) where

import System.Directory (doesFileExist)
import System.IO (hFlush, stdout, hIsTerminalDevice, stdin, isEOF)
import Data.Maybe (isJust, fromJust)

fileExist :: [String] -> IO (Maybe String)
fileExist [] = return Nothing
fileExist (x:xs) = do
    exist <- doesFileExist x
    if exist
        then return (Just x)
    else
        fileExist xs

getParamsLine :: [String] -> String
getParamsLine ("-l":[]) = []
getParamsLine ("-l":xs) = last xs
getParamsLine (x:_) = x
getParamsLine _ = []

getInput :: Bool -> IO ()
getInput flag = do
    isTerminal <- hIsTerminalDevice stdin
    if not isTerminal
        then do
            isEnd <- isEOF
            if isEnd
                then return ()
            else
                getInputLine flag
    else do
        putStr "> "
        hFlush stdout
        getInputLine flag


getInputLine :: Bool -> IO ()
getInputLine flag = do
    line <- getLine
    if flag
        then putStrLn (line ++ " flag") -- compile with flag "-l"
    else
        putStrLn line
    getInput flag

getFile :: Bool -> String -> IO ()
getFile False path = do
    content <- readFile path
    putStrLn content
getFile _ path = do
    content <- readFile path
    putStrLn (content ++ "\nflag") -- compile with flag "-l"

simpleLine :: Bool -> String -> IO ()
simpleLine False str = putStrLn str
simpleLine _ str = putStrLn (str ++ " flag") -- compile with flag "-l"

launch :: [String] -> IO Bool
launch [] = getInput False >> return True
launch args | (length args > 2) = return False
            | (length args == 2) && not ("-l" `elem` args) = return False
            | ("-l" `elem` args) && (length args == 1) = getInput True >> return True
            | otherwise = do
                file <- fileExist args
                if (isJust file)
                    then getFile ("-l" `elem` args) (fromJust file) >> return True
                else
                    simpleLine ("-l" `elem` args) (getParamsLine args) >> return True
