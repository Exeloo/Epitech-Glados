module Main (main) where

import Launch

import System.Exit
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    res <- launch args
    if res
        then exitSuccess
    else
        exitWith (ExitFailure 84)
