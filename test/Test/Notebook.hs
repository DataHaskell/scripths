module Test.Notebook (notebookTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import qualified Data.Text as T

import ScriptHs.Markdown (Segment (..))
import ScriptHs.Notebook (
    addOutputToSegments,
    generatedMarkedScript,
    isHaskell,
    mkIndexedCodeSegments,
    mkMarker,
    parseBlocks,
    processNotebook,
    splitByMarkers,
 )

import ScriptHs.Parser (CabalMeta (metaDeps), Line (..))

notebookTests :: TestTree
notebookTests =
    testGroup
        "Notebook"
        [ testGroup
            "isHaskell"
            [ testCase "accepts haskell" $ do
                isHaskell "haskell" @?= True
            , testCase "accepts hs" $ do
                isHaskell "hs" @?= True
            , testCase "case-insensitive + trims" $ do
                isHaskell "  HaSkElL  " @?= True
                isHaskell "\tHS\n" @?= True
            , testCase "rejects other languages" $ do
                isHaskell "python" @?= False
                isHaskell "" @?= False
                isHaskell "hask" @?= False
            ]
        , testGroup
            "mkMarker"
            [ testCase "format includes index" $ do
                mkMarker 0 @?= "---SCRIPTHS_BLOCK_0_END---"
                mkMarker 12 @?= "---SCRIPTHS_BLOCK_12_END---"
            ]
        , testGroup
            "splitByMarkers"
            [ testCase "empty marker list => []" $ do
                splitByMarkers "anything" [] @?= []
            , testCase "marker not found => strip remaining as output for that idx" $ do
                let out = "  hello world  \n"
                splitByMarkers out [0] @?= [(0, "hello world")]
            , testCase "single marker splits before it" $ do
                let out = T.unlines ["42", mkMarker 0, "ignored trailing"]
                splitByMarkers out [0] @?= [(0, "42")]
            , testCase "multiple markers split sequentially" $ do
                let out =
                        T.concat
                            [ "cell0\n"
                            , mkMarker 0
                            , "\ncell1\n"
                            , mkMarker 1
                            , "\ncell2\n"
                            , mkMarker 2
                            , "\n"
                            ]
                splitByMarkers out [0, 1, 2]
                    @?= [ (0, "cell0")
                        , (1, "cell1")
                        , (2, "cell2")
                        ]
            , testCase "later marker missing => remainder becomes that cell output" $ do
                let out =
                        T.concat
                            [ "cell0\n"
                            , mkMarker 0
                            , "\ncell1-no-marker-at-end\n"
                            ]
                splitByMarkers out [0, 1]
                    @?= [ (0, "cell0")
                        , (1, "cell1-no-marker-at-end")
                        ]
            ]
        , testGroup
            "mkIndexedCodeSegments"
            [ testCase "filters only haskell/hs code blocks" $ do
                let segs =
                        [ (0, Prose "intro\n")
                        , (1, CodeBlock "haskell" "print 1\n" Nothing)
                        , (2, CodeBlock "python" "print('x')\n" Nothing)
                        , (3, CodeBlock "HS" "print 2\n" Nothing)
                        , (4, Prose "outro\n")
                        ]
                let hsSegs = mkIndexedCodeSegments segs
                map fst hsSegs @?= [1, 3]
                case map snd hsSegs of
                    [CodeBlock "haskell" _ _, CodeBlock "HS" _ _] -> pure ()
                    other -> assertFailure $ "unexpected segments: " ++ show other
            ]
        , testGroup
            "addOutputToSegments"
            [ testCase "adds outputs to matching code blocks and leaves others alone" $ do
                let outputs =
                        [ (1, "out-1")
                        , (3, "out-3")
                        , (999, "unused")
                        ]

                let indexedSegs =
                        [ (0, Prose "intro\n")
                        , (1, CodeBlock "haskell" "print 1\n" Nothing)
                        , (2, CodeBlock "python" "print('x')\n" Nothing)
                        , (3, CodeBlock "hs" "print 3\n" (Just "old-should-be-replaced"))
                        , (4, Prose "outro\n")
                        ]

                let result = addOutputToSegments outputs indexedSegs

                length result @?= length indexedSegs

                head result @?= Prose "intro\n"
                result !! 4 @?= Prose "outro\n"

                result !! 1 @?= CodeBlock "haskell" "print 1\n" (Just "out-1")
                result !! 3 @?= CodeBlock "hs" "print 3\n" (Just "out-3")

                result !! 2 @?= CodeBlock "python" "print('x')\n" Nothing
            ]
        , testGroup
            "generatedMarkedScript"
            [ testCase "inserts marker print after each block" $ do
                let blocks =
                        [ (10, [HaskellLine "print 10"])
                        , (11, [HaskellLine "print 11"])
                        ]
                let ls = generatedMarkedScript blocks

                assertBool "has first line" (HaskellLine "print 10" `elem` ls)
                assertBool "has second line" (HaskellLine "print 11" `elem` ls)

                let m10 = mkMarker 10
                    m11 = mkMarker 11

                assertBool
                    "has putStrLn marker 10"
                    (HaskellLine ("putStrLn " <> T.pack (show (T.unpack m10))) `elem` ls)

                assertBool
                    "has putStrLn marker 11"
                    (HaskellLine ("putStrLn " <> T.pack (show (T.unpack m11))) `elem` ls)

                assertBool "has Blank separators" (Blank `elem` ls)
            ]
        , testGroup
            "parseBlocks"
            [ testCase "extracts only haskell blocks and preserves indices" $ do
                let segs =
                        [ (0, Prose "intro\n")
                        , (1, CodeBlock "python" "print('x')\n" Nothing)
                        , (2, CodeBlock "haskell" "print 42\n" Nothing)
                        , (3, CodeBlock "hs" "print 99\n" Nothing)
                        , (4, Prose "outro\n")
                        ]

                let (_meta, indexedBlocks) = parseBlocks segs
                map fst indexedBlocks @?= [2, 3]

                case indexedBlocks of
                    [(2, ls2), (3, ls3)] -> do
                        assertBool "block 2 non-empty lines" (not (null ls2))
                        assertBool "block 3 non-empty lines" (not (null ls3))
                    other -> assertFailure $ "unexpected indexedBlocks: " ++ show other
            , testCase "no haskell blocks => empty indexedBlocks and empty-ish meta" $ do
                let segs =
                        [ (0, Prose "intro\n")
                        , (1, CodeBlock "python" "print('x')\n" Nothing)
                        , (2, Prose "outro\n")
                        ]
                let (meta, indexedBlocks) = parseBlocks segs
                indexedBlocks @?= []
                metaDeps meta @?= []
            ]
        , testGroup
            "processNotebook"
            [ testCase "no code blocks => returns input unchanged" $ do
                let input = T.unlines ["# Title", "", "Just prose.", ""]
                out <- processNotebook input
                out @?= input
            , testCase "non-haskell code blocks only => returns input unchanged" $ do
                let input =
                        T.unlines
                            [ "# Title"
                            , ""
                            , "```python"
                            , "print('hello')"
                            , "```"
                            , ""
                            , "more prose"
                            ]
                out <- processNotebook input
                out @?= input
            ]
        ]
