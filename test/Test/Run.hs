module Test.Run (runTests) where

import Data.List (isPrefixOf)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import ScriptHs.Parser (CabalMeta (..), Line (..), SourceRepoPin (..))
import ScriptHs.Run (
    cabalArgs,
    compileCdTo,
    deriveProjectName,
    renderCabalFile,
    renderCabalProject,
    usesTemplateHaskell,
 )

emptyMeta :: CabalMeta
emptyMeta = CabalMeta [] [] [] [] [] []

runTests :: TestTree
runTests =
    testGroup
        "Run"
        [ testGroup
            "cabalArgs"
            [ testCase "uses -ghci-script= and -e return() for batch mode" $ do
                let args = cabalArgs "/proj" "/proj/script.ghci"
                    replyOpts = filter ("--repl-option=" `isPrefixOf`) args
                replyOpts
                    @?= [ "--repl-option=-ghci-script=/proj/script.ghci"
                        , "--repl-option=-e"
                        , "--repl-option=return()"
                        ]
            , testCase "includes --project-dir" $ do
                let args = cabalArgs "/proj" "/proj/script.ghci"
                assertBool "--project-dir present" $
                    any ("--project-dir=" `isPrefixOf`) args
            ]
        , testGroup
            "renderCabalProject"
            [ testCase "base case has the managed sentinel and packages: ." $ do
                let txt = renderCabalProject [] []
                assertBool "sentinel" (T.isInfixOf "managed by scripths" txt)
                assertBool "packages ." (T.isInfixOf "packages: ." txt)
            , testCase "local package dirs appear as packages entries" $ do
                let txt = renderCabalProject ["/abs/granite"] []
                assertBool "abs path" (T.isInfixOf "/abs/granite" txt)
            , testCase "git pin renders a source-repository-package stanza" $ do
                let pin = SourceRepoPin "https://x/repo" "abc123" (Just "sub")
                    txt = renderCabalProject [] [pin]
                assertBool "stanza" (T.isInfixOf "source-repository-package" txt)
                assertBool "type" (T.isInfixOf "type: git" txt)
                assertBool "location" (T.isInfixOf "location: https://x/repo" txt)
                assertBool "tag" (T.isInfixOf "tag: abc123" txt)
                assertBool "subdir" (T.isInfixOf "subdir: sub" txt)
            ]
        , testGroup
            "deriveProjectName"
            [ testCase "sanitizes non-alphanumerics to dashes" $
                deriveProjectName "/a/b/probe.md" @?= "a-b-probe-md"
            , testCase "collapses runs and trims so a leading _ is valid" $
                -- '/' then '_' would yield '--' (an empty cabal name component).
                deriveProjectName "/x/_probe.md" @?= "x-probe-md"
            , testCase "strips trailing separators" $
                deriveProjectName "/x/note." @?= "x-note"
            ]
        , testGroup
            "renderCabalFile"
            [ testCase "always includes base and directory" $ do
                let txt = T.pack (renderCabalFile "p" [] emptyMeta)
                assertBool "base" (T.isInfixOf "base" txt)
                assertBool "directory" (T.isInfixOf "directory" txt)
            , testCase "enables OverloadedStrings by default" $ do
                let txt = T.pack (renderCabalFile "p" [] emptyMeta)
                assertBool "OverloadedStrings" (T.isInfixOf "OverloadedStrings" txt)
            , testCase "does not duplicate OverloadedStrings if also user-supplied" $ do
                let meta = emptyMeta{metaExts = ["OverloadedStrings"]}
                    n =
                        length (filter (== "OverloadedStrings") (words' (renderCabalFile "p" [] meta)))
                n @?= 1
            , testCase "adds local package names to build-depends" $ do
                let txt = T.pack (renderCabalFile "p" ["granite", "th-helpers"] emptyMeta)
                assertBool "granite dep" (T.isInfixOf "granite" txt)
                assertBool "th-helpers dep" (T.isInfixOf "th-helpers" txt)
            , testCase "dedups names already present in build-depends" $ do
                let meta = emptyMeta{metaDeps = ["granite"]}
                    txt = renderCabalFile "p" ["granite"] meta
                    occurrences = length (filter (== "granite") (words' txt))
                occurrences @?= 1
            ]
        , testGroup
            "usesTemplateHaskell"
            [ testCase "detects the TemplateHaskell extension" $
                usesTemplateHaskell emptyMeta{metaExts = ["TemplateHaskell"]} [] @?= True
            , testCase "detects a template-haskell dependency" $
                usesTemplateHaskell emptyMeta{metaDeps = ["template-haskell"]} [] @?= True
            , testCase "detects a $(...) splice line" $
                usesTemplateHaskell emptyMeta [HaskellLine "$(declareTable db t)"] @?= True
            , testCase "detects the rewritten _ = (); splice form" $
                usesTemplateHaskell emptyMeta [HaskellLine "_ = (); declareTable db t"] @?= True
            , testCase "false for a plain notebook" $
                usesTemplateHaskell emptyMeta [HaskellLine "1 + 1", Import "import Data.List"]
                    @?= False
            ]
        , testGroup
            "compileCdTo"
            [ testCase "renders a compile-time setCurrentDirectory splice" $ do
                let txt = compileCdTo "/some/dir"
                assertBool "splice block" (T.isInfixOf ":{" txt)
                assertBool "chdir call" (T.isInfixOf "setCurrentDirectory" txt)
                assertBool "the target dir" (T.isInfixOf "/some/dir" txt)
            ]
        ]
  where
    -- crude tokenizer: split build-depends on commas/spaces for the dedup check
    words' = words . map (\c -> if c == ',' then ' ' else c)
