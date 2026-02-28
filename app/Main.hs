module Main where

import Data.List (isPrefixOf)
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
        [path] -> dispatch path Nothing
        [] -> usage
        _ -> dispatch (last args) (outputFile (init args))

dispatch :: FilePath -> Maybe FilePath -> IO ()
dispatch path outputPath =
    case takeExtension path of
        ".md" -> runNotebook path outputPath
        ".markdown" -> runNotebook path outputPath
        _ -> do
            contents <- TIO.readFile path
            let sf = parseScript contents
            runScript sf

-- TODO: This is kinda brittle but I don't wanna
-- include the whole opt parser lib just to
-- get the output file. Maybe there's a cheaper
-- way to do this.
outputFile :: [String] -> Maybe FilePath
outputFile [] = Nothing
outputFile ("-o" : f : _) = Just f
outputFile (x : xs) =
    if "--output=" `isPrefixOf` x
        then Just (drop (length "--output=") x)
        else outputFile (drop 1 xs)

usage :: IO ()
usage = do
    prog <- getProgName
    putStrLn $
        "Usage: " ++ prog ++ " [--output=<filename>] [-o <filename>] <script>"
    exitFailure
