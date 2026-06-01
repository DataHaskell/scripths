{-# LANGUAGE OverloadedStrings #-}

{- | The GHCi code scripths injects into the throwaway repl session — the
auto-print preamble, the working-directory and Template Haskell @cd@ splices, and
the batch terminator — plus scrubbing of scripths' own identifiers out of
captured output.

Every injected name is qualified against a base module /other than/ @Prelude@.
Importing @Prelude@ — even @qualified@ — suppresses GHC's implicit @import
Prelude@ and would strip unqualified names from the user's own cells, whereas
importing any other base module does not. That is what lets a notebook run under
@NoImplicitPrelude@ or a custom prelude. The @ScripthsInternal*@ aliases are
listed once in 'scripthsInternalQualifiers' so the generators and
'scrubInternalNames' cannot drift apart.
-}
module ScriptHs.Repl (
    scripthsIO,
    scripthsInternalQualifiers,
    scrubInternalNames,
    autoPrintDirective,
    cdDirective,
    compileCdSetup,
    compileCdTo,
    replEvalExpr,
) where

import qualified Data.Text as T

{- | Qualified alias for base's @System.IO@ in scripths-injected GHCi statements
(autoprint and the notebook block markers). We qualify against @System.IO@ (not
@Prelude@) so the injected code resolves @print@\/@putStrLn@ under
@NoImplicitPrelude@ without suppressing the user's implicit @Prelude@.
-}
scripthsIO :: T.Text
scripthsIO = "ScripthsInternalIO"

{- | Each @ScripthsInternal*@ alias scripths injects, paired with the real base
module it stands for. Single source of truth shared by the generators above and
'scrubInternalNames' so they cannot drift.
-}
scripthsInternalAliases :: [(T.Text, T.Text)]
scripthsInternalAliases =
    [ (scripthsIO, "System.IO")
    , ("ScripthsInternalShow", "Text.Show")
    , ("ScripthsInternalStr", "Data.String")
    , ("ScripthsInternalMonad", "Control.Monad")
    , ("ScripthsInternalApplicative", "Control.Applicative")
    , ("ScripthsInternalTH", "Language.Haskell.TH.Syntax")
    , ("ScripthsInternalDir", "System.Directory")
    ]

-- | The qualified module aliases scripths injects into the GHCi session.
scripthsInternalQualifiers :: [T.Text]
scripthsInternalQualifiers = map fst scripthsInternalAliases

{- | Strip scripths' internal identifiers from captured GHCi output so a user's
diagnostic reads like a vanilla GHCi one rather than exposing the synthetic
preamble:

  * a /qualified/ use loses its qualifier (@ScripthsInternalStr.IsString@ -> @IsString@);
  * a /bare/ alias (e.g. @Could not load module \'ScripthsInternalDir\'@) becomes
    the real module it stands for (@System.Directory@);
  * the auto-print method @scripthsAutoPrint@ becomes @print@ and its class
    @ScripthsAutoPrint@ becomes @Show@ (what GHCi shows without scripths).

These tokens are deliberately exotic, so a blind 'T.replace' over user-visible
output is very unlikely to collide with real content (a cell that literally
prints one of them as data would be rewritten — an accepted trade-off).
-}
scrubInternalNames :: T.Text -> T.Text
scrubInternalNames t0 = foldl (\acc (n, r) -> T.replace n r acc) t0 replacements
  where
    -- Order matters: strip qualified @Alias.@ uses first, then the method and
    -- class names, then map any /remaining/ bare alias to its real module.
    replacements =
        [(a <> ".", "") | (a, _) <- scripthsInternalAliases]
            ++ [ ("scripthsAutoPrint", "print")
               , ("ScripthsAutoPrint", "Show")
               ]
            ++ [(a, m) | (a, m) <- scripthsInternalAliases]

{- | Make GHCi auto-print a trailing 'String' result /raw/ (via 'putStr')
instead of @show@-escaping it into a quoted one-line literal. This lets a
notebook cell end in @df |> toMarkdown'@ and have the markdown actually render,
with no explicit 'putStrLn'. Every non-'String' result still prints via @show@,
so tuples, @Either@, dataframes, etc. are unaffected.

Every name is qualified against a base module other than @Prelude@ (so the
directive compiles under @NoImplicitPrelude@ / a custom prelude without
suppressing the user's implicit @Prelude@).
-}
autoPrintDirective :: T.Text
autoPrintDirective =
    T.unlines
        [ ":set -XFlexibleInstances -XUndecidableInstances"
        , "import qualified System.IO as " <> scripthsIO
        , "import qualified Text.Show as ScripthsInternalShow"
        , "import qualified Data.String as ScripthsInternalStr"
        , "class ScripthsAutoPrint a where scripthsAutoPrint :: a -> "
            <> scripthsIO
            <> ".IO ()"
        , "instance {-# OVERLAPPING #-} ScripthsAutoPrint ScripthsInternalStr.String where scripthsAutoPrint = "
            <> scripthsIO
            <> ".putStr"
        , "instance {-# OVERLAPPABLE #-} ScripthsInternalShow.Show a => ScripthsAutoPrint a where scripthsAutoPrint = "
            <> scripthsIO
            <> ".print"
        , ":set -interactive-print scripthsAutoPrint"
        ]

{- | A GHCi preamble that changes the /process/ working directory to the dir
@scripths@ was invoked from, so a script can read @./data/foo.csv@ relative to
the user's working tree (the throwaway project lives under @~\/.scripths@).

We use @System.Directory.setCurrentDirectory@ rather than GHCi's @:cd@: @:cd@
unloads every loaded module (which breaks the @cabal repl@ session), whereas a
plain IO action does not. @show@ renders a correctly escaped Haskell string
literal, so paths containing spaces or quotes are safe. @directory@ is added to
the synthetic package's dependencies (see @renderCabalFile@) so the import
always resolves.
-}
cdDirective :: FilePath -> T.Text
cdDirective dir =
    T.pack $
        unlines
            [ "import qualified System.Directory as ScripthsInternalDir"
            , "ScripthsInternalDir.setCurrentDirectory " <> show dir
            ]

{- | Enable @TemplateHaskell@ and bring TH's @runIO@ (plus the @(>>)@\/@pure@ the
splice sequences with) into scope for the compile-time chdir splices, all
qualified so they resolve under @NoImplicitPrelude@. Emitted only when the
notebook uses Template Haskell.
-}
compileCdSetup :: T.Text
compileCdSetup =
    T.unlines
        [ ":set -XTemplateHaskell"
        , "import qualified Language.Haskell.TH.Syntax as ScripthsInternalTH"
        , "import qualified Control.Monad as ScripthsInternalMonad"
        , "import qualified Control.Applicative as ScripthsInternalApplicative"
        ]

{- | A GHCi top-level auto-splice (type @Q [Dec]@) running @setCurrentDirectory@
at compile time, moving the process the /later/ splices compile in. Used pointed
at the user's cwd before the blocks and back at the project dir after them.
-}
compileCdTo :: FilePath -> T.Text
compileCdTo dir =
    T.unlines
        [ ":{"
        , "_ = (); ScripthsInternalTH.runIO (ScripthsInternalDir.setCurrentDirectory "
            <> T.pack (show dir)
            <> ") ScripthsInternalMonad.>> ScripthsInternalApplicative.pure []"
        , ":}"
        ]

{- | The expression GHCi evaluates (via @-e@) after sourcing the script, to run
the session non-interactively and exit. A silent @hFlush stdout@ rather than
@return ()@, qualified through 'scripthsIO' (which @-e@ sees from the script's
imports) so it needs no Prelude under @NoImplicitPrelude@ / a custom prelude.
Written @hFlush(stdout)@ with no space: cabal splits a @--repl-option@ value on
whitespace, and Haskell's @f(x)@ juxtaposition keeps it a single application.
-}
replEvalExpr :: T.Text
replEvalExpr = scripthsIO <> ".hFlush(" <> scripthsIO <> ".stdout)"
