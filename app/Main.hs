module Main where

import ScriptHs.Parser

import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    contents <- TIO.readFile "../hscript/examples/analysis.ghci"
    print $ parseScript contents
