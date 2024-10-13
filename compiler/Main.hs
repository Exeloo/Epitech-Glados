module Main (main) where

import Launch

import System.Exit
import System.Environment

help :: IO ()
help = putStrLn "USAGE: ./glados [file/line]\n" >>
       putStrLn "       file       path of the file used" >>
       putStrLn "       line       line of code used" >>
       exitSuccess


main :: IO ()
main = getArgs >>= \args ->
    if "--help" `elem` args
        then help
        else launch args >>= \res ->
            if res
                then exitSuccess
                else exitWith (ExitFailure 84)
