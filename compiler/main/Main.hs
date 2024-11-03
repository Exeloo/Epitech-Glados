module Main (main) where

import Launch

import System.Exit
import System.Environment

help :: IO ()
help = putStrLn "USAGE: ./glados [-f file] [-c]\n" >>
       putStrLn "       -f         path of the file used" >>
       putStrLn "       -c         to compile the program" >>
       exitSuccess

main :: IO ()
main = getArgs >>= \args ->
    if "--help" `elem` args
        then help
        else launch args >>= \res ->
            if res
                then exitSuccess
                else exitWith (ExitFailure 84)
