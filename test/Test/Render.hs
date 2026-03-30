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
        , testGroup
            "Continuation across blanks (don't break)"
            [ testCase "blank between independent IO statements still separates" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "putStrLn \"a\""
                            , Blank
                            , HaskellLine "putStrLn \"b\""
                            ]
                let blocks = splitBlocks result
                assertBool
                    ("expected 2 blocks, got " ++ show (length blocks))
                    (length blocks == 2)
            , testCase "blank between imports still separates" $ do
                let result =
                        toGhciScript
                            [ Import "import Data.Text"
                            , Blank
                            , Import "import Data.Map"
                            ]
                -- Both imports should appear
                assertBool "has Text" (T.isInfixOf "Data.Text" result)
                assertBool "has Map" (T.isInfixOf "Data.Map" result)
            ]
        , testGroup
            "Continuation across blanks (new behavior)"
            [ testCase "blank before where stays in same block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "foo x = bar x"
                            , Blank
                            , HaskellLine "  where"
                            , HaskellLine "    bar = id"
                            ]
                let blocks = splitBlocks result
                assertBool
                    ( "expected 1 block for where clause, got "
                        ++ show (length blocks)
                        ++ ": "
                        ++ show blocks
                    )
                    (length blocks == 1)
            , testCase "blank before guards stays in same block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "f n"
                            , Blank
                            , HaskellLine "  | n > 0 = 1"
                            , HaskellLine "  | otherwise = 0"
                            ]
                let blocks = splitBlocks result
                assertBool
                    ( "expected 1 block for guards, got "
                        ++ show (length blocks)
                        ++ ": "
                        ++ show blocks
                    )
                    (length blocks == 1)
            , testCase "blank before deriving stays in same block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "data Color = Red | Green | Blue"
                            , Blank
                            , HaskellLine "  deriving (Show, Eq)"
                            ]
                let blocks = splitBlocks result
                assertBool
                    ( "expected 1 block for deriving, got "
                        ++ show (length blocks)
                        ++ ": "
                        ++ show blocks
                    )
                    (length blocks == 1)
            , testCase "double blank still breaks (intentional separation)" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "x = 1"
                            , Blank
                            , Blank
                            , HaskellLine "y = 2"
                            ]
                let blocks = splitBlocks result
                assertBool
                    ("expected 2 blocks for double blank, got " ++ show (length blocks))
                    (length blocks == 2)
            ]
        , testGroup
            "Mixed block splitting (don't break)"
            [ testCase "pure definitions stay grouped" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "f x = x + 1"
                            , HaskellLine "g y = y * 2"
                            ]
                let blocks = splitBlocks result
                assertBool
                    ("expected 1 block for definitions, got " ++ show (length blocks))
                    (length blocks == 1)
            , testCase "indented continuation stays grouped" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "f x ="
                            , HaskellLine "  x + 1"
                            ]
                let blocks = splitBlocks result
                length blocks @?= 1
            ]
        , testGroup
            "Mixed block splitting (new behavior)"
            [ testCase "definition then IO splits into two blocks" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "x = 5"
                            , HaskellLine "print x"
                            ]
                let blocks = splitBlocks result
                assertBool
                    ( "expected 2 blocks for def+IO, got "
                        ++ show (length blocks)
                        ++ ": "
                        ++ show blocks
                    )
                    (length blocks == 2)
            , testCase "IO then definition splits" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "putStrLn \"hi\""
                            , HaskellLine "x = 5"
                            ]
                let blocks = splitBlocks result
                assertBool
                    ( "expected 2 blocks for IO+def, got "
                        ++ show (length blocks)
                        ++ ": "
                        ++ show blocks
                    )
                    (length blocks == 2)
            , testCase "three mixed items produce three blocks" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "x = 1"
                            , HaskellLine "print x"
                            , HaskellLine "y = 2"
                            ]
                let blocks = splitBlocks result
                assertBool
                    ("expected 3 blocks, got " ++ show (length blocks) ++ ": " ++ show blocks)
                    (length blocks == 3)
            , testCase "type sig + definition stays together" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "f :: Int -> Int"
                            , HaskellLine "f x = x + 1"
                            ]
                let blocks = splitBlocks result
                assertBool
                    ("expected 1 block for sig+def, got " ++ show (length blocks))
                    (length blocks == 1)
            ]
        , testGroup
            "Bracket counting (don't break)"
            [ testCase "complete expression in parens" $ do
                let result = toGhciScript [HaskellLine "f x = (x + 1)"]
                let blocks = splitBlocks result
                length blocks @?= 1
            , testCase "balanced multiline already grouped by indentation" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "f x ="
                            , HaskellLine "  (x + 1)"
                            ]
                let blocks = splitBlocks result
                length blocks @?= 1
            ]
        , testGroup
            "Bracket counting (new behavior)"
            [ testCase "unclosed bracket extends block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "xs = ["
                            , HaskellLine "  1,"
                            , HaskellLine "  2,"
                            , HaskellLine "  3"
                            , HaskellLine "  ]"
                            ]
                let blocks = splitBlocks result
                assertBool
                    ( "expected 1 block for list literal, got "
                        ++ show (length blocks)
                        ++ ": "
                        ++ show blocks
                    )
                    (length blocks == 1)
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
