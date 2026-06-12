module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Test.Compiled (compiledTests)
import Test.Integration (integrationTests)
import Test.Markdown (markdownTests)
import Test.Notebook (notebookTests)
import Test.Parser (parseTests)
import Test.Render (renderTests)
import Test.Repl (replTests)
import Test.Run (runTests)
import Test.Version (versionTests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "ScriptHs"
            [ parseTests
            , compiledTests
            , renderTests
            , markdownTests
            , notebookTests
            , replTests
            , runTests
            , versionTests
            , integrationTests
            ]
