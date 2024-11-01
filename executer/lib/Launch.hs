module Launch (launch) where

import Parser (parseSExpr)
import ExecInstructions (exec)
import System.IO (hPutStrLn, stderr)
import System.Directory (doesFileExist)
import Text.Megaparsec
import SExprToInstructions (sExprToInsts)
import InstructionData (Insts)


execFile :: Insts -> IO Bool
execFile insts =
    case exec insts [] insts [] of
        Left err -> hPutStrLn stderr ("Exec error: " ++ err) >> return False
        Right res -> print res >> return True

parseFile :: String -> IO Bool
parseFile file =
    case parse parseSExpr "" file of
        Left err -> hPutStrLn stderr ("Parse error: " ++ errorBundlePretty err) >> return False
        Right res ->
            case sExprToInsts res of
                Left err -> hPutStrLn stderr ("Parse error: " ++ err) >> return False
                Right insts -> execFile insts

launch :: [String] -> IO Bool
launch [path] = doesFileExist path >>= \exist ->
    if exist
        then readFile path >>= \file -> parseFile file
        else putStrLn ("File" ++ path ++ "do not exist") >> return False
launch _ = hPutStrLn stderr "Invalid args" >> return False
