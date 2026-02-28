module Test.Render (renderTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

import Data.Text (Text)
import qualified Data.Text as T
import ScriptHs.Parser (Line (..))
import ScriptHs.Render (toGhciScript)

renderTests :: TestTree
renderTests =
    testGroup
        "Render"
        [ testGroup
            "Single lines"
            [ testCase "plain expression stays unwrapped" $ do
                let result = toGhciScript [HaskellLine "print 42"]
                assertWrapped result ["print 42"]
            , testCase "import stays unwrapped" $ do
                let result = toGhciScript [Import "import Data.Text"]
                assertNotWrapped result
            , testCase "pragma stays unwrapped" $ do
                let result = toGhciScript [Import "{-# LANGUAGE GADTs #-}"]
                assertNotWrapped result
            , testCase "ghci command stays unwrapped" $ do
                let result = toGhciScript [GhciCommand ":set -XOverloadedStrings"]
                let ls = nonEmpty result
                ls @?= [":set -XOverloadedStrings"]
            , testCase "IO bind gets wrapped" $ do
                let result = toGhciScript [HaskellLine "x <- getLine"]
                assertWrapped result ["x <- getLine"]
            , testCase "blank line preserved" $ do
                let result = toGhciScript [Blank]
                assertBool "has blank" (T.isInfixOf "\n\n" result || result == "\n")
            ]
        , testGroup
            "IO line isolation"
            [ testCase "consecutive IO binds each get own block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "x <- getLine"
                            , HaskellLine "y <- getLine"
                            ]
                let blocks = splitBlocks result
                assertBool
                    ("expected 2 blocks, got " ++ show (length blocks))
                    (length blocks == 2)
            , testCase "IO bind between pure code splits correctly" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "let x = 5"
                            , HaskellLine "y <- getLine"
                            , HaskellLine "print y"
                            ]
                let blocks = splitBlocks result
                assertBool
                    ("expected 3 blocks, got " ++ show (length blocks) ++ ": " ++ show blocks)
                    (length blocks == 3)
            ]
        , testGroup
            "Multi-line blocks"
            [ testCase "consecutive pure lines grouped" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "let"
                            , HaskellLine "  x = 5"
                            , HaskellLine "  y = 10"
                            ]
                let blocks = splitBlocks result
                length blocks @?= 1
            , testCase "consecutive do-notation lines grouped" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "do"
                            , HaskellLine "  x <- pure 5"
                            , HaskellLine "  y <- pure 10"
                            , HaskellLine "  pure $ x + y"
                            ]
                let blocks = splitBlocks result
                length blocks @?= 1
            , testCase "consecutive do-notation lines with space are grouped" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "do"
                            , HaskellLine "  x <- pure 5"
                            , -- indentation should ignore these blank lines.
                              Blank
                            , Blank
                            , HaskellLine "  y <- pure 10"
                            , HaskellLine "  pure $ x + y"
                            ]
                let blocks = splitBlocks result
                length blocks @?= 1
            , testCase "blank separates blocks" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "print 1"
                            , Blank
                            , HaskellLine "print 2"
                            ]
                let ls = nonEmpty result
                assertBool "has print 1" (any (T.isInfixOf "print 1") ls)
                assertBool "has print 2" (any (T.isInfixOf "print 2") ls)
            ]
        , testGroup
            "Full script transform"
            [ testCase "typical script" $ do
                let result =
                        toGhciScript
                            [ Import "import qualified DataFrame as D"
                            , Blank
                            , HaskellLine "iris <- D.readParquet \"data/iris.parquet\""
                            , Blank
                            , Import "import Data.Text (Text)"
                            , Blank
                            , HaskellLine "print iris"
                            ]
                assertBool "has import D" (T.isInfixOf "import qualified DataFrame as D" result)
                assertBool "has :{" (T.isInfixOf ":{" result)
                assertBool "has print" (T.isInfixOf "print iris" result)
            ]
        ]

splitBlocks :: Text -> [[Text]]
splitBlocks = go . T.lines
  where
    go [] = []
    go (l : ls)
        | T.strip l == ":{" =
            let (block, rest) = span (\x -> T.strip x /= ":}") ls
                rest' = drop 1 rest
             in block : go rest'
        | otherwise = go ls

nonEmpty :: Text -> [Text]
nonEmpty = filter (not . T.null . T.strip) . T.lines

assertNotWrapped :: Text -> Assertion
assertNotWrapped t =
    assertBool
        ("expected no wrapping, got: " ++ T.unpack t)
        (not (T.isInfixOf ":{" t))

assertWrapped :: Text -> [Text] -> Assertion
assertWrapped t expectedInner = do
    let blocks = splitBlocks t
    assertBool
        ("expected at least one block, got: " ++ show blocks)
        (not (null blocks))
    let innerLines = map T.strip (concat blocks)
    let expected = map T.strip expectedInner
    assertBool
        ("expected " ++ show expected ++ " in blocks, got: " ++ show innerLines)
        (all (`elem` innerLines) expected)
