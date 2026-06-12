module Test.Compiled (compiledTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Data.Text (Text)
import qualified Data.Text as T
import ScriptHs.Compiled (
    CellChunk (..),
    CompileIssue (..),
    checkCompilable,
    isValidModuleName,
    linePragmaTag,
    parseLinePragmaTag,
    renderCompiledModule,
 )
import ScriptHs.Parser (
    CompileDirective (..),
    ScriptFile (scriptCompile, scriptLines),
    Line (..),
    parseScript,
    parseScriptNumbered,
 )

numbered :: Text -> [(Int, Line)]
numbered = snd . parseScriptNumbered

compiledTests :: TestTree
compiledTests =
    testGroup
        "Compiled"
        [ directiveTests
        , numberedTests
        , checkTests
        , nameTests
        , tagTests
        , renderTests
        ]

directiveTests :: TestTree
directiveTests =
    testGroup
        "compile directive"
        [ testCase "absent by default" $
            scriptCompile (parseScript "x = 1\n") @?= Nothing
        , testCase "bare directive" $
            scriptCompile (parseScript "-- compile\nx = 1\n")
                @?= Just CompileDefault
        , testCase "named directive" $
            scriptCompile (parseScript "-- compile: Training\nx = 1\n")
                @?= Just (CompileNamed "Training")
        , testCase "dotted name" $
            scriptCompile (parseScript "-- compile: Training.Core\nx = 1\n")
                @?= Just (CompileNamed "Training.Core")
        , testCase "directive removed from script lines" $
            scriptLines (parseScript "-- compile\nx = 1\n")
                @?= [HaskellLine "x = 1"]
        , testCase "directive after cabal lines" $ do
            let sf = parseScript "-- cabal: build-depends: text\n-- compile\nx = 1\n"
            scriptCompile sf @?= Just CompileDefault
        , testCase "whitespace tolerated" $
            scriptCompile (parseScript "--  compile:  Training \nx = 1\n")
                @?= Just (CompileNamed "Training")
        , testCase "unrelated comment is not a directive" $
            scriptCompile (parseScript "-- compiled fast\nx = 1\n") @?= Nothing
        ]

numberedTests :: TestTree
numberedTests =
    testGroup
        "parseScriptNumbered"
        [ testCase "plain lines numbered from 1" $
            numbered "x = 1\ny = 2\n"
                @?= [(1, HaskellLine "x = 1"), (2, HaskellLine "y = 2")]
        , testCase "directive lines keep following numbering" $
            numbered "-- cabal: build-depends: text\n-- compile\nx = 1\n"
                @?= [(3, HaskellLine "x = 1")]
        , testCase "agrees with parseScript lines" $ do
            let src = "-- compile\nimport Data.List\n\nf :: Int -> Int\nf x = x\n"
            map snd (numbered src) @?= scriptLines (parseScript src)
        ]

checkTests :: TestTree
checkTests =
    testGroup
        "checkCompilable"
        [ testCase "declarations pass" $
            checkCompilable (numbered "f :: Int -> Int\nf x = x + 1\n") @?= []
        , testCase "imports and pragmas pass" $
            checkCompilable
                (numbered "{-# LANGUAGE LambdaCase #-}\nimport Data.List\n")
                @?= []
        , testCase "data declaration passes" $
            checkCompilable (numbered "data T = A | B\n") @?= []
        , testCase "bare expression rejected with its line" $ do
            let issues = checkCompilable (numbered "f x = x\n\nprint 3\n")
            map ciLine issues @?= [3]
        , testCase "monadic bind rejected" $ do
            let issues = checkCompilable (numbered "x <- readLn\n")
            map ciLine issues @?= [1]
        , testCase ":set -X allowed, other ghci commands rejected" $ do
            checkCompilable (numbered ":set -XLambdaCase\nf x = x\n") @?= []
            map ciLine (checkCompilable (numbered ":type f\nf x = x\n")) @?= [1]
        , testCase "TH splice allowed" $
            checkCompilable (numbered "$(pure [])\n") @?= []
        ]

nameTests :: TestTree
nameTests =
    testGroup
        "isValidModuleName"
        [ testCase "simple" $ isValidModuleName "Training" @?= True
        , testCase "dotted" $ isValidModuleName "Training.Core" @?= True
        , testCase "lowercase rejected" $ isValidModuleName "training" @?= False
        , testCase "empty segment rejected" $ isValidModuleName "A..B" @?= False
        , testCase "empty rejected" $ isValidModuleName "" @?= False
        , testCase "spaces rejected" $ isValidModuleName "A B" @?= False
        ]

tagTests :: TestTree
tagTests =
    testGroup
        "line pragma tags"
        [ testCase "round trip" $
            parseLinePragmaTag (linePragmaTag 42) @?= Just 42
        , testCase "non-tag rejected" $
            parseLinePragmaTag "Training" @?= Nothing
        ]

renderTests :: TestTree
renderTests =
    testGroup
        "renderCompiledModule"
        [ testCase "module header and decl with LINE pragma" $ do
            let src = renderCompiledModule "Training" [] [] [chunk 7 "f :: Int -> Int\nf x = x + 1\n"]
            assertContains src "module Training where"
            assertContains src ("{-# LINE 1 \"" <> linePragmaTag 7 <> "\" #-}")
            assertContains src "f x = x + 1"
        , testCase "default extensions become pragmas" $ do
            let src = renderCompiledModule "M" ["LambdaCase"] [] [chunk 1 "f x = x\n"]
            assertContains src "{-# LANGUAGE LambdaCase #-}"
        , testCase "imports hoisted with their own pragma and line" $ do
            let src =
                    renderCompiledModule
                        "M"
                        []
                        []
                        [chunk 3 "f x = x\nimport Data.List (sort)\ng y = sort y\n"]
            assertContains src ("{-# LINE 2 \"" <> linePragmaTag 3 <> "\" #-}")
            assertContains src "import Data.List (sort)"
            -- imports appear before declarations
            assertBool "import before decl" $
                textIndex "import Data.List" src < textIndex "f x = x" src
        , testCase "extra imports rendered before cell imports" $ do
            let src = renderCompiledModule "M" [] ["import Helpers"] [chunk 1 "f x = x\n"]
            assertContains src "import Helpers"
        , testCase ":set -X contributes a pragma" $ do
            let src = renderCompiledModule "M" [] [] [chunk 1 ":set -XGADTs\nf x = x\n"]
            assertContains src "{-# LANGUAGE GADTs #-}"
        , testCase "pragmas deduped" $ do
            let src =
                    renderCompiledModule
                        "M"
                        ["GADTs"]
                        []
                        [chunk 1 "{-# LANGUAGE GADTs #-}\nf x = x\n"]
            T.count "LANGUAGE GADTs" src @?= 1
        , testCase "unused-import warnings suppressed" $ do
            let src = renderCompiledModule "M" [] [] [chunk 1 "f x = x\n"]
            assertContains src "{-# OPTIONS_GHC -Wno-unused-imports #-}"
        , testCase "two chunks keep their own tags" $ do
            let src =
                    renderCompiledModule
                        "M"
                        []
                        []
                        [chunk 1 "f x = x\n", chunk 2 "g y = y\n"]
            assertContains src (linePragmaTag 1)
            assertContains src (linePragmaTag 2)
        ]
  where
    chunk cid src = CellChunk{ccTag = linePragmaTag cid, ccLines = numbered src}
    assertContains hay needle =
        assertBool (T.unpack ("missing: " <> needle <> "\nin:\n" <> hay)) $
            needle `T.isInfixOf` hay
    textIndex needle hay = T.length (fst (T.breakOn needle hay))
