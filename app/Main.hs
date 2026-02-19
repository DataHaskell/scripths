module Main where

import qualified Data.Text.IO as TIO
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath (takeExtension)

import ScriptHs.Notebook (runNotebook)
import ScriptHs.Parser (parseScript)
import ScriptHs.Run (runScript)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> dispatch path
        _ -> usage

dispatch :: FilePath -> IO ()
dispatch path =
    case takeExtension path of
        ".md" -> runNotebook path
        ".markdown" -> runNotebook path
        _ -> do
            contents <- TIO.readFile path
            let sf = parseScript contents
            runScript sf

usage :: IO ()
usage = do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " <script>"
    exitFailure
