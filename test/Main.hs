module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Test.Markdown (markdownTests)
import Test.Notebook (notebookTests)
import Test.Parser (parseTests)
import Test.Render (renderTests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "ScriptHs"
            [ parseTests
            , renderTests
            , markdownTests
            , notebookTests
            ]
