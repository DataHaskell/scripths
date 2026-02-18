module Main where

import qualified Data.Text.IO as TIO
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import ScriptHs.Parser (parseScript)
import ScriptHs.Run (runScript)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            contents <- TIO.readFile path
            let sf = parseScript contents
            runScript sf
        _ -> usage

usage :: IO ()
usage = do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " <script>"
    exitFailure
