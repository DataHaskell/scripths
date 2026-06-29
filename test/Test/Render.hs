module Test.Render (renderTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

import Data.Text (Text)
import qualified Data.Text as T
import ScriptHs.Parser (CabalMeta (..), Line (..))
import ScriptHs.Render (
    LhsBlock (..),
    ModuleParts (..),
    TrailKind (..),
    actionExprs,
    renderCabalScriptHeader,
    renderLiterate,
    renderModuleText,
    toGhciScript,
    toGhciScriptTagged,
    toModule,
 )

renderTests :: TestTree
renderTests =
    testGroup
        "Render"
        [ testGroup
            "Line-level rendering"
            [ testCase "plain expression stays unwrapped" $ do
                let result = toGhciScript [HaskellLine "print 42"]
                assertWrapped result ["print 42"]
            , testCase "import stays unwrapped" $ do
                let result = toGhciScript [Import "import Data.Text"]
                assertNotWrapped result
            , testCase "pragma stays unwrapped" $ do
                let result = toGhciScript [Pragma "{-# LANGUAGE GADTs #-}"]
                assertNotWrapped result
            , testCase "ghci command stays unwrapped" $ do
                let result = toGhciScript [GhciCommand ":set -XOverloadedStrings"]
                nonEmpty result @?= [":set -XOverloadedStrings"]
            , testCase "IO bind gets wrapped" $ do
                let result = toGhciScript [HaskellLine "x <- getLine"]
                assertWrapped result ["x <- getLine"]
            , testCase "blank line preserved" $ do
                let result = toGhciScript [Blank]
                assertBool "has blank" (T.isInfixOf "\n\n" result || result == "\n")
            ]
        , testGroup
            "Isolation: one block per statement"
            [ testCase "consecutive IO binds each get own block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "x <- getLine"
                            , HaskellLine "y <- getLine"
                            ]
                length (splitBlocks result) @?= 2
            , testCase "consecutive IO expression-statements each get own block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "print 1"
                            , HaskellLine "putStrLn \"hi\""
                            , HaskellLine "displayLatex \"hi\""
                            , HaskellLine "displayLatex \"hi\""
                            ]
                length (splitBlocks result) @?= 4
            , testCase "inline type annotation does not flip statement into declaration" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "print (1 :: Int)"
                            , HaskellLine "print \"x\""
                            ]
                length (splitBlocks result) @?= 2
            , testCase "TH splice gets its own block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "$(deriveJSON defaultOptions ''Foo)"
                            , HaskellLine "print \"after\""
                            ]
                length (splitBlocks result) @?= 2
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
                length (splitBlocks result) @?= 1
            , testCase "consecutive do-notation lines grouped" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "do"
                            , HaskellLine "  x <- pure 5"
                            , HaskellLine "  y <- pure 10"
                            , HaskellLine "  pure $ x + y"
                            ]
                length (splitBlocks result) @?= 1
            , testCase "consecutive do-notation lines with space are grouped" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "do"
                            , HaskellLine "  x <- pure 5"
                            , Blank
                            , Blank
                            , HaskellLine "  y <- pure 10"
                            , HaskellLine "  pure $ x + y"
                            ]
                length (splitBlocks result) @?= 1
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
            "Continuation across blanks"
            [ testCase "blank between independent IO statements still separates" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "putStrLn \"a\""
                            , Blank
                            , HaskellLine "putStrLn \"b\""
                            ]
                length (splitBlocks result) @?= 2
            , testCase "blank between imports still separates" $ do
                let result =
                        toGhciScript
                            [ Import "import Data.Text"
                            , Blank
                            , Import "import Data.Map"
                            ]
                assertBool "has Text" (T.isInfixOf "Data.Text" result)
                assertBool "has Map" (T.isInfixOf "Data.Map" result)
            , testCase "blank before where stays in same block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "foo x = bar x"
                            , Blank
                            , HaskellLine "  where"
                            , HaskellLine "    bar = id"
                            ]
                length (splitBlocks result) @?= 1
            , testCase "blank before guards stays in same block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "f n"
                            , Blank
                            , HaskellLine "  | n > 0 = 1"
                            , HaskellLine "  | otherwise = 0"
                            ]
                length (splitBlocks result) @?= 1
            , testCase "blank before deriving stays in same block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "data Color = Red | Green | Blue"
                            , Blank
                            , HaskellLine "  deriving (Show, Eq)"
                            ]
                length (splitBlocks result) @?= 1
            , testCase "double blank still breaks (intentional separation)" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "x = 1"
                            , Blank
                            , Blank
                            , HaskellLine "y = 2"
                            ]
                length (splitBlocks result) @?= 2
            , testCase "type sig + single blank + binding stay in one block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "f :: Int -> Int"
                            , Blank
                            , HaskellLine "f x = x + 1"
                            ]
                length (splitBlocks result) @?= 1
            , testCase "two value bindings + single blank stay in one block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "x = 1"
                            , Blank
                            , HaskellLine "y = 2"
                            ]
                length (splitBlocks result) @?= 1
            , testCase "data decl + single blank + instance stay in one block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "data User = User { name :: String }"
                            , Blank
                            , HaskellLine "instance Show User where"
                            , HaskellLine "  show u = name u"
                            ]
                length (splitBlocks result) @?= 1
            ]
        , testGroup
            "Mixed block splitting"
            [ testCase "pure definitions stay grouped" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "f x = x + 1"
                            , HaskellLine "g y = y * 2"
                            ]
                length (splitBlocks result) @?= 1
            , testCase "indented continuation stays grouped" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "f x ="
                            , HaskellLine "  x + 1"
                            ]
                length (splitBlocks result) @?= 1
            , testCase "definition then IO splits into two blocks" $ do
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
                length (splitBlocks result) @?= 1
            , testCase "comment + type sig + guarded body + where stays together" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "-- Helper function to check if a number is prime"
                            , HaskellLine "isPrime :: Int -> Bool"
                            , HaskellLine "isPrime n"
                            , HaskellLine "  | n < 2 = False"
                            , HaskellLine "  | n == 2 = True"
                            , HaskellLine "  | even n = False"
                            , HaskellLine "  | otherwise = all (\\k -> n `mod` k /= 0) [3, 5 .. isqrt n]"
                            , HaskellLine "  where isqrt = floor . sqrt . fromIntegral"
                            ]
                length (splitBlocks result) @?= 1
            , testCase "multi-section script: commented + typed functions + do block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "-- Helper function to check if a number is prime"
                            , HaskellLine "isPrime :: Int -> Bool"
                            , HaskellLine "isPrime n"
                            , HaskellLine "  | n < 2 = False"
                            , HaskellLine "  | n == 2 = True"
                            , HaskellLine "  | even n = False"
                            , HaskellLine "  | otherwise = all (\\k -> n `mod` k /= 0) [3, 5 .. isqrt n]"
                            , HaskellLine "  where isqrt = floor . sqrt . fromIntegral"
                            , Blank
                            , HaskellLine "-- Generate list of primes up to n"
                            , HaskellLine "primesUpTo :: Int -> [Int]"
                            , HaskellLine "primesUpTo n = filter isPrime [2 .. n]"
                            , Blank
                            , HaskellLine "-- Compute gaps between consecutive primes"
                            , HaskellLine "primeGaps :: [Int] -> [Int]"
                            , HaskellLine "primeGaps ps = zipWith (-) (tail ps) ps"
                            , Blank
                            , HaskellLine "-- Test"
                            , HaskellLine "do"
                            , HaskellLine "  let testPrimes = primesUpTo 10"
                            ]
                let blocks = splitBlocks result
                assertBool
                    ( "expected 4 blocks (one per logical section), got "
                        ++ show (length blocks)
                        ++ ": "
                        ++ show blocks
                    )
                    (length blocks == 4)
            ]
        , testGroup
            "Comments"
            [ testCase "comment attaches to following declaration" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "-- doc comment"
                            , HaskellLine "f x = x + 1"
                            ]
                length (splitBlocks result) @?= 1
                assertWrapped result ["-- doc comment", "f x = x + 1"]
            ]
        , testGroup
            "Bracket counting"
            [ testCase "complete expression in parens" $ do
                let result = toGhciScript [HaskellLine "f x = (x + 1)"]
                length (splitBlocks result) @?= 1
            , testCase "balanced multiline already grouped by indentation" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "f x ="
                            , HaskellLine "  (x + 1)"
                            ]
                length (splitBlocks result) @?= 1
            , testCase "unclosed bracket extends block" $ do
                let result =
                        toGhciScript
                            [ HaskellLine "xs = ["
                            , HaskellLine "  1,"
                            , HaskellLine "  2,"
                            , HaskellLine "  3"
                            , HaskellLine "  ]"
                            ]
                length (splitBlocks result) @?= 1
            ]
        , taggedTests
        , moduleTests
        ]

{- | 'toGhciScriptTagged' prefixes every @:{ … :}@ block with a
@{\-# LINE n "tag" #-\}@ pragma so GHC (esp. @-fdiagnostics-as-json@) reports
cell-relative lines and the cell's file tag, neutralizing any preamble GHCi
prepends. Directives, imports, pragmas and blanks stay bare (a LINE pragma
cannot attach to them at the prompt).
-}
taggedTests :: TestTree
taggedTests =
    testGroup
        "Tagged rendering (LINE pragmas)"
        [ testCase "single statement gets a LINE pragma with its source line + tag" $ do
            let result = toGhciScriptTagged "cell" [(3, HaskellLine "print 42")]
            linePragmasIn result @?= ["{-# LINE 3 \"cell\" #-}"]
        , testCase "the LINE pragma is the first line inside the :{ block" $ do
            let result = toGhciScriptTagged "cell" [(3, HaskellLine "print 42")]
            case splitBlocks result of
                (block : _) ->
                    map T.strip block @?= ["{-# LINE 3 \"cell\" #-}", "print 42"]
                [] -> assertBool "expected a block" False
        , testCase "directives, imports, pragmas and blanks are not tagged" $ do
            let result =
                    toGhciScriptTagged
                        "cell"
                        [ (1, Import "import Data.Text")
                        , (2, Blank)
                        , (3, Pragma "{-# LANGUAGE GADTs #-}")
                        , (4, GhciCommand ":set -XOverloadedStrings")
                        ]
            linePragmasIn result @?= []
        , testCase "merged type sig + binding share one pragma at the first line" $ do
            let result =
                    toGhciScriptTagged
                        "cell"
                        [ (5, HaskellLine "f :: Int -> Int")
                        , (6, HaskellLine "f x = x + 1")
                        ]
            length (splitBlocks result) @?= 1
            linePragmasIn result @?= ["{-# LINE 5 \"cell\" #-}"]
        , testCase "comment attaching forward uses the comment's line number" $ do
            let result =
                    toGhciScriptTagged
                        "cell"
                        [ (2, HaskellLine "-- doc comment")
                        , (3, HaskellLine "g y = y * 2")
                        ]
            length (splitBlocks result) @?= 1
            linePragmasIn result @?= ["{-# LINE 2 \"cell\" #-}"]
        , testCase "each statement block gets its own pragma at its own line" $ do
            let result =
                    toGhciScriptTagged
                        "cell"
                        [ (1, HaskellLine "x = 1")
                        , (2, HaskellLine "print x")
                        ]
            length (splitBlocks result) @?= 2
            linePragmasIn result @?= ["{-# LINE 1 \"cell\" #-}", "{-# LINE 2 \"cell\" #-}"]
        , testCase "line numbers come from the source, not the rendered position" $ do
            let result =
                    toGhciScriptTagged
                        "sabela-cell-7"
                        [(42, HaskellLine "boom")]
            linePragmasIn result @?= ["{-# LINE 42 \"sabela-cell-7\" #-}"]
        ]

linePragmasIn :: Text -> [Text]
linePragmasIn =
    filter (T.isPrefixOf "{-# LINE") . map T.strip . T.lines

moduleTests :: TestTree
moduleTests =
    testGroup
        "Module rendering (notebook export)"
        [ testCase "pragmas and imports hoist into their buckets" $ do
            let mp =
                    toModule
                        (const TrailUnknown)
                        [ Pragma "{-# LANGUAGE GADTs #-}"
                        , Import "import Data.Text (Text)"
                        , HaskellLine "x = 5"
                        ]
            mpPragmas mp @?= ["{-# LANGUAGE GADTs #-}"]
            mpImports mp @?= ["import Data.Text (Text)"]
            mpDecls mp @?= ["x = 5"]
            mpMain mp @?= []
        , testCase ":set -XExt becomes LANGUAGE pragma(s)" $ do
            let mp =
                    toModule (const TrailUnknown) [GhciCommand ":set -XOverloadedStrings -XGADTs"]
            mpPragmas mp
                @?= ["{-# LANGUAGE OverloadedStrings #-}", "{-# LANGUAGE GADTs #-}"]
        , testCase "non -X ghci directives and :{ :} are dropped" $ do
            let mp =
                    toModule
                        (const TrailUnknown)
                        [GhciCommand ":type foo", GhciCommand ":{", GhciCommand ":}"]
            mp @?= mempty
        , testCase "value binding routes to decls, not main" $ do
            let mp = toModule (const TrailUnknown) [HaskellLine "f x = x + 1"]
            mpDecls mp @?= ["f x = x + 1"]
            mpMain mp @?= []
        , testCase "IO bind routes to main verbatim" $ do
            let mp = toModule (const TrailUnknown) [HaskellLine "x <- getLine"]
            mpMain mp @?= ["x <- getLine"]
            mpDecls mp @?= []
        , testCase "trailing pure expression is printed" $ do
            let mp = toModule (const TrailPure) [HaskellLine "df"]
            mpMain mp @?= ["print (df)"]
        , testCase "trailing IO () expression is verbatim" $ do
            let mp = toModule (const TrailIOUnit) [HaskellLine "putStrLn \"hi\""]
            mpMain mp @?= ["putStrLn \"hi\""]
        , testCase "trailing IO-show expression binds with print" $ do
            let mp = toModule (const TrailIOShow) [HaskellLine "readLn"]
            mpMain mp @?= ["print =<< (readLn)"]
        , testCase "unresolved trailing expression is commented out" $ do
            let mp = toModule (const TrailUnknown) [HaskellLine "mystery"]
            assertBool "comments out the expr" (any (T.isInfixOf "-- mystery") (mpMain mp))
        , testCase "TH splice is un-rewritten to a top-level decl" $ do
            let mp = toModule (const TrailUnknown) [HaskellLine "_ = (); deriveFoo ''Bar"]
            mpDecls mp @?= ["$(deriveFoo ''Bar)"]
        , testCase "actionExprs lists trailing expressions in order" $
            actionExprs [HaskellLine "putStrLn \"a\"", HaskellLine "result"]
                @?= ["putStrLn \"a\"", "result"]
        , testCase "empty main renders as pure ()" $ do
            let txt = renderModuleText (Just "Main") (mempty{mpDecls = ["x = 1"]})
            assertBool "has module header" (T.isInfixOf "module Main where" txt)
            assertBool "has main = pure ()" (T.isInfixOf "main = pure ()" txt)
        , testCase "main do-block indents statements" $ do
            let txt = renderModuleText (Just "Main") (mempty{mpMain = ["print 1", "print 2"]})
            assertBool "has main = do" (T.isInfixOf "main = do" txt)
            assertBool "indents print 1" (T.isInfixOf "    print 1" txt)
        , testCase "cabal-script header includes base + Wno-unused-imports" $ do
            let hdr =
                    renderCabalScriptHeader (CabalMeta ["dataframe", "text"] [] [] [] [] [] [] [])
            assertBool "opens block" (T.isInfixOf "{- cabal:" hdr)
            assertBool
                "base + deps"
                (T.isInfixOf "build-depends: base, dataframe, text" hdr)
            assertBool "suppresses unused imports" (T.isInfixOf "-Wno-unused-imports" hdr)
        , testCase "literate output separates code from prose with a blank line" $
            renderLiterate [LhsProse "Intro paragraph.", LhsCode ["main = pure ()"]]
                @?= "Intro paragraph.\n\n> main = pure ()"
        , testCase
            "literate prose indents lines starting with # or > (else GHC mis-reads them)"
            $ do
                let txt =
                        renderLiterate
                            [LhsProse "# Heading\n> quote\nplain", LhsCode ["main = pure ()"]]
                assertBool "escapes leading #" (T.isPrefixOf " # Heading" txt)
                assertBool "escapes leading >" (T.isInfixOf "\n > quote" txt)
                assertBool "leaves plain prose" (T.isInfixOf "\nplain" txt)
                assertBool "code still bird-prefixed" (T.isInfixOf "> main = pure ()" txt)
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
