module Main (main) where

import Launch

import System.Exit
import System.Environment

main :: IO ()
main = getArgs >>= \args ->
    if "--help" `elem` args
        then exitSuccess
        else launch args >>= \res ->
            if res
                then exitSuccess
                else exitWith (ExitFailure 84)
