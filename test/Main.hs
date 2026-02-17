module Main (main) where

import Test.Tasty

import Test.Parser (parseTests)
import Test.Render (renderTests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "ScriptHs"
            [ parseTests
            , renderTests
            ]
