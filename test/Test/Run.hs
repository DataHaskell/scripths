module Test.Run (runTests) where

import Data.List (isPrefixOf)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import ScriptHs.Parser (SourceRepoPin (..))
import ScriptHs.Run (cabalArgs, renderCabalProject)

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
        ]
