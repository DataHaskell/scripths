module Test.Parser (parseTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (
    assertBool,
    assertFailure,
    testCase,
    (@?=),
 )

import qualified Data.Text as T
import ScriptHs.Parser (
    CabalMeta (metaDeps, metaExts, metaGhcOptions),
    Line (..),
    ScriptFile (scriptLines, scriptMeta),
    parseScript,
 )

parseTests :: TestTree
parseTests =
    testGroup
        "Parse"
        [ testGroup
            "Line classification"
            [ testCase "blank line" $ do
                let sf = parseScript "\n"
                scriptLines <$> sf @?= Right [Blank]
            , testCase "import" $ do
                let sf = parseScript "import Data.Text (Text)\n"
                case scriptLines <$> sf of
                    Right [Import t] -> assertBool "import text" (T.isPrefixOf "import " t)
                    other -> assertFailure $ "expected Import, got: " ++ show other
            , testCase "qualified import" $ do
                let sf = parseScript "import qualified Data.Map as Map\n"
                case scriptLines <$> sf of
                    Right [Import t] -> assertBool "qualified" (T.isInfixOf "qualified" t)
                    other -> assertFailure $ "expected Import, got: " ++ show other
            , testCase "ghci command :set" $ do
                let sf = parseScript ":set -XOverloadedStrings\n"
                case scriptLines <$> sf of
                    Right [GhciCommand t] -> t @?= ":set -XOverloadedStrings"
                    other -> assertFailure $ "expected GhciCommand, got: " ++ show other
            , testCase "ghci command :def!" $ do
                let sf = parseScript ":def! declareColumns \\s -> return s\n"
                case scriptLines <$> sf of
                    Right [GhciCommand t] -> assertBool ":def!" (T.isPrefixOf ":def!" t)
                    other -> assertFailure $ "expected GhciCommand, got: " ++ show other
            , testCase "pragma" $ do
                let sf = parseScript "{-# LANGUAGE TemplateHaskell #-}\n"
                case scriptLines <$> sf of
                    Right [Pragma t] -> assertBool "pragma" (T.isPrefixOf "{-#" t)
                    other -> assertFailure $ "expected Pragma, got: " ++ show other
            , testCase "haskell line" $ do
                let sf = parseScript "print (5 + 5)\n"
                case scriptLines <$> sf of
                    Right [HaskellLine t] -> t @?= "print (5 + 5)"
                    other -> assertFailure $ "expected HaskellLine, got: " ++ show other
            , testCase "IO bind line" $ do
                let sf = parseScript "x <- getLine\n"
                case scriptLines <$> sf of
                    Right [HaskellLine t] -> assertBool "has <-" (T.isInfixOf "<-" t)
                    other -> assertFailure $ "expected HaskellLine, got: " ++ show other
            , testCase "TH splice line" $ do
                let sf = parseScript "$(declareColumns iris)\n"
                case scriptLines <$> sf of
                    Right [HaskellLine t] -> assertBool "has $(" (T.isPrefixOf "_ = ();" t)
                    other -> assertFailure $ "expected HaskellLine, got: " ++ show other
            ]
        , testGroup
            "Cabal metadata"
            [ testCase "build-depends" $ do
                let sf = parseScript "-- cabal: build-depends: base, text, containers\n"
                metaDeps . scriptMeta <$> sf @?= Right ["base", "text", "containers"]
            , testCase "default-extensions" $ do
                let sf =
                        parseScript "-- cabal: default-extensions: TemplateHaskell, TypeApplications\n"
                metaExts . scriptMeta <$> sf @?= Right ["TemplateHaskell", "TypeApplications"]
            , testCase "ghc-options" $ do
                let sf = parseScript "-- cabal: ghc-options: -threaded, -O2\n"
                metaGhcOptions . scriptMeta <$> sf @?= Right ["-threaded", "-O2"]
            , testCase "metadata stripped from lines" $ do
                let input =
                        T.unlines
                            [ "-- cabal: build-depends: base"
                            , "import Data.Text"
                            ]
                let sf = parseScript input
                length . scriptLines <$> sf @?= Right 1
                case scriptLines <$> sf of
                    Right [Import _] -> pure ()
                    other -> assertFailure $ "expected [Import], got: " ++ show other
            , testCase "multiple metadata lines merge" $ do
                let input =
                        T.unlines
                            [ "-- cabal: build-depends: base, text"
                            , "-- cabal: build-depends: containers"
                            , "-- cabal: default-extensions: GADTs"
                            ]
                let sf = parseScript input
                metaDeps . scriptMeta <$> sf @?= Right ["base", "text", "containers"]
                metaExts . scriptMeta <$> sf @?= Right ["GADTs"]
            , testCase "unknown cabal key is ignored" $ do
                let sf = parseScript "-- cabal: foo: bar, baz\n"
                metaDeps . scriptMeta <$> sf @?= Right []
                metaExts . scriptMeta <$> sf @?= Right []
            ]
        , testGroup
            "Multi-line scripts"
            [ testCase "interleaved imports and expressions" $ do
                let input =
                        T.unlines
                            [ "import Data.Text (Text)"
                            , ""
                            , "x <- getLine"
                            , ""
                            , "import Data.Map (Map)"
                            , ""
                            , "print x"
                            ]
                let sf = either error id (parseScript input)
                let ls = scriptLines sf
                length ls @?= 7 -- 3 code + 4 blanks (trailing newline)
                case filter notBlank ls of
                    [Import _, HaskellLine _, Import _, HaskellLine _] -> pure ()
                    other -> assertFailure $ "unexpected structure: " ++ show other
            , testCase "empty input" $ do
                let sf = either error id $ parseScript ""
                scriptLines sf @?= []
                metaDeps (scriptMeta sf) @?= []
            , testCase "no trailing newline" $ do
                let sf = either error id $ parseScript "print 42"
                case scriptLines sf of
                    [HaskellLine t] -> t @?= "print 42"
                    other -> assertFailure $ "expected HaskellLine, got: " ++ show other
            ]
        , testGroup
            "Edge cases"
            [ testCase "comment that looks like cabal but isn't" $ do
                let sf = either error id $ parseScript "-- cabal is great\n"

                case scriptLines sf of
                    [HaskellLine _] -> pure ()
                    other -> assertFailure $ "expected HaskellLine, got: " ++ show other
            , testCase "regular comment" $ do
                let sf = either error id $ parseScript "-- this is a comment\n"
                case scriptLines sf of
                    [HaskellLine t] -> assertBool "comment" (T.isPrefixOf "--" t)
                    other -> assertFailure $ "expected HaskellLine, got: " ++ show other
            , testCase "indented import is haskell line" $ do
                let sf = either error id $ parseScript "  import Data.Text\n"
                case scriptLines sf of
                    [HaskellLine _] -> pure ()
                    other -> assertFailure $ "expected HaskellLine, got: " ++ show other
            ]
        ]

notBlank :: Line -> Bool
notBlank Blank = False
notBlank _ = True
