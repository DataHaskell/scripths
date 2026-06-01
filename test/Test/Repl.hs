module Test.Repl (replTests) where

import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import ScriptHs.Repl (
    autoPrintDirective,
    compileCdSetup,
    compileCdTo,
    replEvalExpr,
    scripthsInternalQualifiers,
    scrubInternalNames,
 )

replTests :: TestTree
replTests =
    testGroup
        "Repl"
        [ compileCdToTests
        , preludeAgnosticTests
        , scrubTests
        ]

compileCdToTests :: TestTree
compileCdToTests =
    testGroup
        "compileCdTo"
        [ testCase "renders a compile-time setCurrentDirectory splice" $ do
            let txt = compileCdTo "/some/dir"
            assertBool "splice block" (T.isInfixOf ":{" txt)
            assertBool "chdir call" (T.isInfixOf "setCurrentDirectory" txt)
            assertBool "the target dir" (T.isInfixOf "/some/dir" txt)
        , testCase "sequences via qualified >>/pure, not bare Prelude" $ do
            let txt = compileCdTo "/some/dir"
            assertBool "qualified (>>)" (T.isInfixOf "ScripthsInternalMonad.>>" txt)
            assertBool "qualified pure" (T.isInfixOf "ScripthsInternalApplicative.pure" txt)
            assertBool "no bare Prelude" (not (T.isInfixOf "Prelude." txt))
        ]

preludeAgnosticTests :: TestTree
preludeAgnosticTests =
    testGroup
        "prelude-agnostic injection"
        -- Every name scripths injects into the repl is qualified against a base
        -- module other than Prelude, so a notebook under NoImplicitPrelude / a
        -- custom prelude still runs (and the implicit Prelude is not suppressed).
        [ testCase "autoPrintDirective qualifies through System.IO, not Prelude" $ do
            assertBool "imports System.IO qualified" $
                T.isInfixOf
                    "import qualified System.IO as ScripthsInternalIO"
                    autoPrintDirective
            assertBool "sets the auto-print" $
                T.isInfixOf "-interactive-print scripthsAutoPrint" autoPrintDirective
            assertBool "never mentions Prelude" $
                not (T.isInfixOf "Prelude" autoPrintDirective)
        , testCase "compileCdSetup imports the sequencing modules qualified" $ do
            assertBool "Control.Monad" $
                T.isInfixOf
                    "import qualified Control.Monad as ScripthsInternalMonad"
                    compileCdSetup
            assertBool "Control.Applicative" $
                T.isInfixOf
                    "import qualified Control.Applicative as ScripthsInternalApplicative"
                    compileCdSetup
        , testCase "the -e terminator is prelude-free and space-free" $ do
            assertBool
                "uses qualified hFlush"
                (T.isInfixOf "ScripthsInternalIO.hFlush" replEvalExpr)
            assertBool "no Prelude" (not (T.isInfixOf "Prelude" replEvalExpr))
            assertBool "no return" (not (T.isInfixOf "return" replEvalExpr))
            assertBool "no spaces" (not (T.isInfixOf " " replEvalExpr))
        ]

scrubTests :: TestTree
scrubTests =
    testGroup
        "scrubInternalNames"
        -- Captured GHCi diagnostics must read like vanilla GHCi, never exposing
        -- the synthetic preamble's identifiers.
        [ testCase "rewrites scripthsAutoPrint to print" $ do
            let s =
                    scrubInternalNames
                        "arising from a use of scripthsAutoPrint\nscripthsAutoPrint it"
            assertBool "no scripthsAutoPrint" (not (T.isInfixOf "scripthsAutoPrint" s))
            assertBool "reads as print it" (T.isInfixOf "print it" s)
        , testCase "strips an internal qualifier (the IsString leak)" $
            scrubInternalNames "No instance for (ScripthsInternalStr.IsString Int)"
                @?= "No instance for (IsString Int)"
        , testCase "removes every internal qualifier" $ do
            let sample = T.unwords [q <> ".foo" | q <- scripthsInternalQualifiers]
            assertBool
                "no ScripthsInternal remains"
                (not (T.isInfixOf "ScripthsInternal" (scrubInternalNames sample)))
        , testCase "leaves ordinary output untouched" $
            scrubInternalNames "Variable not in scope: frobnicate"
                @?= "Variable not in scope: frobnicate"
        ]
