module Test.Run (runTests) where

import Data.List (isPrefixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import ScriptHs.Run (cabalArgs)

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
        ]
