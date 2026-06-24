module Test.Run (runTests) where

import Data.Char (isAsciiLower, isDigit)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import ScriptHs.Parser (CabalMeta (..), Line (..), SourceRepoPin (..))
import ScriptHs.Run (
    cabalArgs,
    deriveProjectName,
    renderCabalFile,
    renderCabalProject,
    scriptGhciBody,
    usesTemplateHaskell,
 )

emptyMeta :: CabalMeta
emptyMeta = CabalMeta [] [] [] [] [] [] [] []

runTests :: TestTree
runTests =
    testGroup
        "Run"
        [ testGroup
            "cabalArgs"
            [ testCase "uses -ghci-script= and a prelude-free -e terminator for batch mode" $ do
                let args = cabalArgs "/proj" "/proj/script.ghci"
                    replyOpts = filter ("--repl-option=" `isPrefixOf`) args
                replyOpts
                    @?= [ "--repl-option=-ghci-script=/proj/script.ghci"
                        , "--repl-option=-e"
                        , "--repl-option=ScripthsInternalIO.hFlush(ScripthsInternalIO.stdout)"
                        ]
            , testCase
                "the -e terminator has no spaces (cabal splits --repl-option on whitespace)"
                $ do
                    let args = cabalArgs "/proj" "/proj/script.ghci"
                        eTerm = last (filter ("--repl-option=" `isPrefixOf`) args)
                    assertBool "no space in terminator" (' ' `notElem` eTerm)
            , testCase "includes --project-dir" $ do
                let args = cabalArgs "/proj" "/proj/script.ghci"
                assertBool "--project-dir present" $
                    any ("--project-dir=" `isPrefixOf`) args
            ]
        , testGroup
            "renderCabalProject"
            [ testCase "base case has the managed sentinel and packages: ." $ do
                let txt = renderCabalProject [] [] [] []
                assertBool "sentinel" (T.isInfixOf "managed by scripths" txt)
                assertBool "packages ." (T.isInfixOf "packages: ." txt)
            , testCase "local package dirs appear as packages entries" $ do
                let txt = renderCabalProject ["/abs/granite"] [] [] []
                assertBool "abs path" (T.isInfixOf "/abs/granite" txt)
            , testCase "git pin renders a source-repository-package stanza" $ do
                let pin = SourceRepoPin "https://x/repo" "abc123" (Just "sub")
                    txt = renderCabalProject [] [pin] [] []
                assertBool "stanza" (T.isInfixOf "source-repository-package" txt)
                assertBool "type" (T.isInfixOf "type: git" txt)
                assertBool "location" (T.isInfixOf "location: https://x/repo" txt)
                assertBool "tag" (T.isInfixOf "tag: abc123" txt)
                assertBool "subdir" (T.isInfixOf "subdir: sub" txt)
            , testCase "no package * stanza when no extra dirs are declared" $ do
                let txt = renderCabalProject [] [] [] []
                assertBool "no package *" (not (T.isInfixOf "package *" txt))
            , testCase "extra dirs render a package * stanza (applies to deps)" $ do
                let txt =
                        renderCabalProject
                            []
                            []
                            ["/opt/homebrew/opt/opencascade/lib"]
                            ["/opt/homebrew/opt/opencascade/include/opencascade"]
                assertBool "package * header" (T.isInfixOf "package *" txt)
                assertBool
                    "lib dir"
                    (T.isInfixOf "    extra-lib-dirs: /opt/homebrew/opt/opencascade/lib" txt)
                assertBool
                    "inc dir"
                    ( T.isInfixOf
                        "    extra-include-dirs: /opt/homebrew/opt/opencascade/include/opencascade"
                        txt
                    )
            ]
        , testGroup
            "deriveProjectName"
            [ testCase "sanitises to a readable stem, then a -h<hash> suffix" $ do
                let n = deriveProjectName "/a/b/probe.md"
                assertBool "readable stem" ("a-b-probe-md-h" `isPrefixOf` n)
                assertBool "no double dash" (not ("--" `isInfixOf` n))
            , testCase "collapses runs and trims so a leading _ is valid" $
                -- '/' then '_' would yield '--' (an empty cabal name component).
                assertBool "stem" ("x-probe-md-h" `isPrefixOf` deriveProjectName "/x/_probe.md")
            , testCase "distinct paths that sanitise alike do NOT collide" $
                -- foo.md and foo-md both sanitise to home-u-foo-md; the hash keeps
                -- them in separate ~/.scripths dirs.
                assertBool "differ" $
                    deriveProjectName "/home/u/foo.md" /= deriveProjectName "/home/u/foo-md"
            , testCase "is stable for the same path" $
                deriveProjectName "/home/u/x.md" @?= deriveProjectName "/home/u/x.md"
            , testCase "non-ASCII letters become dashes (cabal needs ASCII names)" $ do
                let n = deriveProjectName "/проект/данные.md"
                assertBool
                    "ASCII only"
                    (all (\c -> c == '-' || isAsciiLower c || isDigit c) n)
            , testCase "an all-symbol path still yields a valid name" $
                assertBool
                    "fallback stem"
                    ("scripths-script-h" `isPrefixOf` deriveProjectName "/---")
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
            "scriptGhciBody"
            [ testCase "wraps a cell with a LINE pragma naming the script path + source line" $ do
                let body = scriptGhciBody "./examples/analysis.ghci" [(11, HaskellLine "duble 5")]
                assertBool
                    "tagged pragma"
                    (T.isInfixOf "{-# LINE 11 \"./examples/analysis.ghci\" #-}" body)
            , testCase "uses the original source line, not the rendered position" $ do
                let body =
                        scriptGhciBody
                            "f.ghci"
                            [(1, Import "import Data.Text"), (5, HaskellLine "x = 1")]
                assertBool "statement at line 5" (T.isInfixOf "{-# LINE 5 \"f.ghci\" #-}" body)
            , testCase "imports stay bare (a LINE pragma cannot attach at the prompt)" $ do
                let body =
                        scriptGhciBody
                            "f.ghci"
                            [(1, Import "import Data.Text"), (5, HaskellLine "x = 1")]
                assertBool "import not pragma'd" (not (T.isInfixOf "{-# LINE 1 " body))
            ]
        ]
  where
    -- crude tokenizer: split build-depends on commas/spaces for the dedup check
    words' = words . map (\c -> if c == ',' then ' ' else c)
