module Test.Integration (integrationTests) where

import qualified Data.Text as T
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import ScriptHs.Markdown
import ScriptHs.Notebook (processNotebook)
import ScriptHs.Run (defaultRunOptions)

{- | Out-of-band integration test: it actually drives @cabal repl@ (slow, needs a
toolchain), so it is gated behind @SCRIPTHS_INTEGRATION=1@ and is a no-op in the
default fast/pure suite. It is the only test that proves the injected preamble
genuinely /compiles and runs/ under @NoImplicitPrelude@ — what the string-level
"prelude-agnostic" checks in "Test.Repl" can only approximate — and that the
rendered notebook leaks none of scripths' internals end-to-end.
-}
integrationTests :: TestTree
integrationTests =
    testGroup
        "Integration (gated by SCRIPTHS_INTEGRATION=1)"
        [ testCase "a NoImplicitPrelude notebook runs and renders, no internals leaked" $ do
            gated <- lookupEnv "SCRIPTHS_INTEGRATION"
            case gated of
                Nothing -> pure () -- skipped in the default suite
                Just _ -> do
                    let nb =
                            T.unlines
                                [ "# Integration"
                                , ""
                                , "```haskell"
                                , "-- cabal: default-extensions: NoImplicitPrelude"
                                , "import Prelude (String)"
                                , "\"rendered-ok\" :: String"
                                , "```"
                                ]
                    out <-
                        processNotebook
                            defaultOutputStyle
                            defaultCodeStyle
                            defaultRunOptions
                            "/tmp/scripths-integration.md"
                            nb
                    assertBool "renders the cell value" ("rendered-ok" `T.isInfixOf` out)
                    assertBool
                        "no ScripthsInternal leak"
                        (not ("ScripthsInternal" `T.isInfixOf` out))
                    assertBool
                        "no scripthsAutoPrint leak"
                        (not ("scripthsAutoPrint" `T.isInfixOf` out))
                    assertBool "no block-marker leak" (not ("SCRIPTHS_BLOCK" `T.isInfixOf` out))
                    assertBool "no compile error" (not ("error:" `T.isInfixOf` out))
        ]
