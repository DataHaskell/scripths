module Test.Markdown (markdownTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (
    assertBool,
    assertFailure,
    testCase,
    (@?=),
 )

import Data.Text (Text)
import qualified Data.Text as T
import ScriptHs.Markdown (
    Segment (CodeBlock, Prose),
    parseMarkdown,
    reassemble,
 )

markdownTests :: TestTree
markdownTests =
    testGroup
        "Markdown"
        [ testGroup
            "parseMarkdown"
            [ testCase "prose only" $ do
                let segs = parseMarkdown "Hello world\nSecond line\n"
                length segs @?= 1
                case segs of
                    [Prose _] -> pure ()
                    other -> assertFailure $ "expected [Prose], got: " ++ show other
            , testCase "single code block" $ do
                let input =
                        T.unlines
                            [ "```haskell"
                            , "print 42"
                            , "```"
                            ]
                let segs = parseMarkdown input
                length segs @?= 1
                case segs of
                    [CodeBlock lang code Nothing] -> do
                        lang @?= "haskell"
                        assertBool "has print 42" (T.isInfixOf "print 42" code)
                    other -> assertFailure $ "expected [CodeBlock], got: " ++ show other
            , testCase "prose then code" $ do
                let input =
                        T.unlines
                            [ "# Title"
                            , ""
                            , "Some text."
                            , ""
                            , "```haskell"
                            , "print 42"
                            , "```"
                            ]
                let segs = parseMarkdown input
                length segs @?= 2
                case segs of
                    [Prose _, CodeBlock "haskell" _ _] -> pure ()
                    other -> assertFailure $ "expected [Prose, CodeBlock], got: " ++ show other
            , testCase "code then prose" $ do
                let input =
                        T.unlines
                            [ "```haskell"
                            , "print 42"
                            , "```"
                            , ""
                            , "Some text after."
                            ]
                let segs = parseMarkdown input
                length segs @?= 2
                case segs of
                    [CodeBlock{}, Prose _] -> pure ()
                    other -> assertFailure $ "expected [CodeBlock, Prose], got: " ++ show other
            , testCase "multiple code blocks" $ do
                let input =
                        T.unlines
                            [ "# Title"
                            , ""
                            , "```haskell"
                            , "print 1"
                            , "```"
                            , ""
                            , "Middle text."
                            , ""
                            , "```haskell"
                            , "print 2"
                            , "```"
                            ]
                let segs = parseMarkdown input
                let codeBlocks = [c | c@(CodeBlock{}) <- segs]
                length codeBlocks @?= 2
            , testCase "non-haskell code block preserved" $ do
                let input =
                        T.unlines
                            [ "```python"
                            , "print('hello')"
                            , "```"
                            ]
                let segs = parseMarkdown input
                case segs of
                    [CodeBlock "python" code Nothing] ->
                        assertBool "has python code" (T.isInfixOf "print('hello')" code)
                    other -> assertFailure $ "expected [CodeBlock python], got: " ++ show other
            , testCase "empty code block" $ do
                let input =
                        T.unlines
                            [ "```haskell"
                            , "```"
                            ]
                let segs = parseMarkdown input
                case segs of
                    [CodeBlock "haskell" code _] ->
                        assertBool "empty or whitespace" (T.null (T.strip code))
                    other -> assertFailure $ "expected [CodeBlock], got: " ++ show other
            , testCase "multi-line code block" $ do
                let input =
                        T.unlines
                            [ "```haskell"
                            , "import Data.Text"
                            , ""
                            , "x <- getLine"
                            , "print x"
                            , "```"
                            ]
                let segs = parseMarkdown input
                case segs of
                    [CodeBlock "haskell" code _] -> do
                        assertBool "has import" (T.isInfixOf "import Data.Text" code)
                        assertBool "has x <- getLine" (T.isInfixOf "x <- getLine" code)
                        assertBool "has print x" (T.isInfixOf "print x" code)
                    other -> assertFailure $ "expected [CodeBlock], got: " ++ show other
            ]
        , testGroup
            "reassemble"
            [ testCase "prose without output" $ do
                let result = reassemble [Prose "Hello\n"]
                result @?= "Hello\n"
            , testCase "code block without output" $ do
                let result = reassemble [CodeBlock "haskell" "print 42\n" Nothing]
                assertBool "has fence" (T.isInfixOf "```haskell" result)
                assertBool "has code" (T.isInfixOf "print 42" result)
                assertBool "no blockquote" (not $ T.isInfixOf "> " result)
            , testCase "code block with output" $ do
                let result = reassemble [CodeBlock "haskell" "print 42\n" (Just "42")]
                assertBool "has fence" (T.isInfixOf "```haskell" result)
                assertBool "has code" (T.isInfixOf "print 42" result)
                assertBool "has blockquote" (T.isInfixOf "> 42" result)
            , testCase "code block with multi-line output" $ do
                let result = reassemble [CodeBlock "hs" "print [1,2]\n" (Just "1\n2")]
                assertBool "has > 1" (T.isInfixOf "> 1" result)
                assertBool "has > 2" (T.isInfixOf "> 2" result)
            , testCase "empty output is omitted" $ do
                let result = reassemble [CodeBlock "haskell" "import X\n" (Just "")]
                assertBool "no blockquote" (not $ T.isInfixOf "> " result)
            , testCase "whitespace-only output is omitted" $ do
                let result = reassemble [CodeBlock "haskell" "import X\n" (Just "  \n  \n")]
                assertBool "no blockquote" (not $ T.isInfixOf "> " result)
            , testCase "full document roundtrip" $ do
                let segs =
                        [ Prose "# Title\n\n"
                        , CodeBlock "haskell" "print 42\n" (Just "42")
                        , Prose "\nSome text.\n"
                        , CodeBlock "haskell" "print 99\n" (Just "99")
                        ]
                let result = reassemble segs

                assertBool "has title" (T.isInfixOf "# Title" result)
                assertBool "has first output" (T.isInfixOf "> 42" result)
                assertBool "has middle text" (T.isInfixOf "Some text." result)
                assertBool "has second output" (T.isInfixOf "> 99" result)

                let titleIdx = indexOf "# Title" result
                    code1Idx = indexOf "print 42" result
                    middleIdx = indexOf "Some text." result
                    code2Idx = indexOf "print 99" result
                assertBool "title before code1" (titleIdx < code1Idx)
                assertBool "code1 before middle" (code1Idx < middleIdx)
                assertBool "middle before code2" (middleIdx < code2Idx)
            ]
        , testGroup
            "blockquote"
            [ testCase "single line" $ do
                let result = reassemble [CodeBlock "hs" "x\n" (Just "hello")]
                assertBool "blockquoted" (T.isInfixOf "> hello" result)
            , testCase "empty lines in output get bare >" $ do
                let result = reassemble [CodeBlock "hs" "x\n" (Just "a\n\nb")]
                assertBool "has bare >" (T.isInfixOf "\n>\n" result)
            ]
        ]

indexOf :: Text -> Text -> Int
indexOf needle haystack =
    let (before, _) = T.breakOn needle haystack
     in T.length before
