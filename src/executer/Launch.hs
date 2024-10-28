module Launch (launch) where

import Parser (parseSExpr)
import ExecInstructions (exec)
import System.IO (hPutStrLn, stderr)
import System.Directory (doesFileExist)
import Text.Megaparsec

execFile :: String -> IO Bool
execFile file =
    case parse parseSExpr "" file of
        Left err -> putStrLn ("Parse error: " ++ errorBundlePretty err) >> return False
        Right res -> print res >> return True

launch :: [String] -> IO Bool
launch [path] = doesFileExist path >>= \exist ->
    if exist
        then readFile path >>= \file -> execFile file
        else putStrLn ("File" ++ path ++ "do not exist") >> return False
launch _ = hPutStrLn stderr "Invalid args" >> return False
