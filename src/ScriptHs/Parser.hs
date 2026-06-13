{-# LANGUAGE OverloadedStrings #-}

{- | Parsing for @.ghci@ scripts understood by ScriptHs.

A script is a sequence of lines, optionally preceded by cabal metadata
directives that declare dependencies, language extensions, and GHC options.

Example script:

@
-- cabal: build-depends: containers
-- cabal: default-extensions: OverloadedStrings
import qualified Data.Map as M

M.fromList (zip [1..10] [2,4..])
@
-}
module ScriptHs.Parser (
    ScriptFile (..),
    CabalMeta (..),
    CompileDirective (..),
    SourceRepoPin (..),
    Line (..),
    parseScript,
    parseScriptNumbered,
    mergeMetas,
) where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import ScriptHs.Version (TagStyle (ScriptTag), parseTagLine)

{- | A fully parsed script, consisting of aggregated cabal metadata and an
ordered list of code lines.
-}
data ScriptFile = ScriptFile
    { scriptMeta :: CabalMeta
    -- ^ Aggregated metadata from all @-- cabal:@ directives in the script.
    , scriptCompile :: Maybe CompileDirective
    -- ^ The first @-- compile@ directive in the script, if any.
    , scriptLines :: [Line]
    -- ^ The code lines of the script, in order, with metadata lines removed.
    }
    deriving (Show, Eq)

{- | A @-- compile@ directive marking a script (notebook cell) as compiled:
its declarations are destined for a generated module rather than the GHCi
prompt. @-- compile: Some.Module@ names the target module explicitly.
-}
data CompileDirective
    = CompileDefault
    | CompileNamed Text
    deriving (Show, Eq)

{- | Cabal metadata extracted from @-- cabal:@ directives.

Multiple directives of the same kind are merged:

@
-- cabal: build-depends: text
-- cabal: build-depends: containers
@

produces @CabalMeta { metaDeps = [\"text\", \"containers\"], ... }@.
-}
data CabalMeta = CabalMeta
    { metaDeps :: [Text]
    -- ^ Packages from @build-depends@ directives.
    , metaExts :: [Text]
    -- ^ Extensions from @default-extensions@ directives.
    , metaGhcOptions :: [Text]
    -- ^ Flags from @ghc-options@ directives.
    , metaExtraLibDirs :: [Text]
    -- ^ Native library search paths from @extra-lib-dirs@ directives.
    , metaExtraIncludeDirs :: [Text]
    -- ^ C header search paths from @extra-include-dirs@ directives.
    , metaPackages :: [Text]
    {- ^ Extra local package dirs from @packages@ directives, resolved relative
    to the script file (lets a notebook depend on a second, non-enclosing local
    package, e.g. @-- cabal: packages: ../sibling@).
    -}
    , metaSourceRepos :: [SourceRepoPin]
    -- ^ Pinned git packages from @source-repository-package@ directives.
    , metaUnknownKeys :: [Text]
    -- ^ Unrecognised @-- cabal:@ directive keys (surfaced as warnings).
    }
    deriving (Show, Eq)

{- | A git @source-repository-package@ pin, declared with

@
-- cabal: source-repository-package: <location> <ref> [subdir]
@

so documentation can build against a pinned, pushed commit/tag (reproducible
and commit-safe, unlike a local working-tree path).
-}
data SourceRepoPin = SourceRepoPin
    { srpLocation :: Text
    -- ^ Repository location (e.g. a git URL).
    , srpRef :: Text
    -- ^ Commit hash or tag (becomes cabal's @tag:@).
    , srpSubdir :: Maybe Text
    -- ^ Optional subdirectory within the repository.
    }
    deriving (Show, Eq)

-- | A single logical line from the script body.
data Line
    = -- | An empty or whitespace-only line.
      Blank
    | -- | A GHCi directive, e.g. @:set -XOverloadedStrings@ or a @:{@ \/ @:}@ block.
      GhciCommand Text
    | -- | A GHC language pragma, e.g. @{\-# LANGUAGE OverloadedStrings #-\}@.
      Pragma Text
    | -- | A Haskell import declaration, e.g. @import Data.Text (Text)@.
      Import Text
    | -- | Any other Haskell source line.
      HaskellLine Text
    deriving (Show, Eq)

{- | Parse a ScriptHs script from 'Text'.

Returns a 'ScriptFile' on success, or a human-readable error message on
failure.

Example:

>>> parseScript "-- cabal: build-depends: text\nimport Data.Text (Text)\n"
ScriptFile {scriptMeta = CabalMeta {metaDeps = ["text"], metaExts = [], metaGhcOptions = [], metaExtraLibDirs = [], metaExtraIncludeDirs = [], metaPackages = [], metaSourceRepos = [], metaUnknownKeys = []}, scriptLines = [Import "import Data.Text (Text)"]}

>>> parseScript ""
ScriptFile {scriptMeta = CabalMeta {metaDeps = [], metaExts = [], metaGhcOptions = [], metaExtraLibDirs = [], metaExtraIncludeDirs = [], metaPackages = [], metaSourceRepos = [], metaUnknownKeys = []}, scriptLines = []}
-}
parseScript :: Text -> ScriptFile
parseScript = fst . parseScriptNumbered

{- | Like 'parseScript', but also returns the code lines paired with their
1-based line numbers in the original input (directive lines are removed from
the stream but the numbering of the survivors is preserved). Used to emit
@{\-# LINE #-\}@ pragmas that point back at the original source.
-}
parseScriptNumbered :: Text -> (ScriptFile, [(Int, Line)])
parseScriptNumbered input =
    let numberedRaw = zip [1 ..] (T.lines input)
        afterTag = dropLeadingVersionTag numberedRaw
        parsed = [(i, parseLine t) | (i, t) <- afterTag]
        metas = [m | (_, RawCabalMeta m) <- parsed]
        compileDir = case [d | (_, RawCompile d) <- parsed] of
            (d : _) -> Just d
            [] -> Nothing
        code = [(i, c) | (i, RawCode c) <- parsed]
        sf =
            ScriptFile
                { scriptMeta = mergeMetas metas
                , scriptCompile = compileDir
                , scriptLines = map snd code
                }
     in (sf, code)

{- | Drop a leading scripths version tag (the first non-blank line, when it is
one) so it is not emitted into the repl. Recognised via the single parser in
"ScriptHs.Version" — so an arbitrary @-- scripths:@ comment elsewhere in the body
is left untouched, and the recognizer cannot drift from the one used to read the
tag.
-}
dropLeadingVersionTag :: [(Int, Text)] -> [(Int, Text)]
dropLeadingVersionTag ls = case span (isBlank . snd) ls of
    (blanks, (_, tag) : rest) | isJust (parseTagLine ScriptTag tag) -> blanks ++ rest
    _ -> ls
  where
    isBlank = T.null . T.strip

data RawLine
    = RawCabalMeta CabalMeta
    | RawCompile CompileDirective
    | RawCode Line

mergeMetas :: [CabalMeta] -> CabalMeta
mergeMetas ms =
    CabalMeta
        { metaDeps = concatMap metaDeps ms
        , metaExts = concatMap metaExts ms
        , metaGhcOptions = concatMap metaGhcOptions ms
        , metaExtraLibDirs = concatMap metaExtraLibDirs ms
        , metaExtraIncludeDirs = concatMap metaExtraIncludeDirs ms
        , metaPackages = concatMap metaPackages ms
        , metaSourceRepos = concatMap metaSourceRepos ms
        , metaUnknownKeys = concatMap metaUnknownKeys ms
        }

parseLine :: Text -> RawLine
parseLine line
    | Just meta <- parseCabalMeta line = RawCabalMeta meta
    | Just dir <- parseCompileDirective line = RawCompile dir
    | otherwise = RawCode (parseCodeLine line)

{- | Recognize a @-- compile@ \/ @-- compile: Some.Module@ directive line.
A comment merely starting with the word (e.g. @-- compiled fast@) is not a
directive; the name is taken verbatim (validation happens downstream).
-}
parseCompileDirective :: Text -> Maybe CompileDirective
parseCompileDirective line = do
    rest <- T.stripPrefix "--" (T.stripStart line)
    after <- T.stripPrefix "compile" (T.stripStart rest)
    let a = T.strip after
    if T.null a
        then Just CompileDefault
        else CompileNamed . T.strip <$> T.stripPrefix ":" a

parseCabalMeta :: Text -> Maybe CabalMeta
parseCabalMeta line = do
    rest <- T.stripPrefix "-- cabal:" line
    let rest' = T.stripStart rest
    case T.break (== ':') rest' of
        (key, colonAndValue) | not (T.null colonAndValue) -> do
            let value = T.stripStart (T.drop 1 colonAndValue)
                items = map T.strip (T.splitOn "," value)
            pure $ case T.strip key of
                "build-depends" -> emptyCabal{metaDeps = items}
                "default-extensions" -> emptyCabal{metaExts = items}
                "ghc-options" -> emptyCabal{metaGhcOptions = items}
                "extra-lib-dirs" -> emptyCabal{metaExtraLibDirs = items}
                "extra-include-dirs" -> emptyCabal{metaExtraIncludeDirs = items}
                "packages" -> emptyCabal{metaPackages = items}
                "source-repository-package" ->
                    emptyCabal{metaSourceRepos = maybeToList (parseSourceRepo value)}
                other -> emptyCabal{metaUnknownKeys = [other]}
        _ -> Nothing
  where
    emptyCabal =
        CabalMeta
            { metaDeps = []
            , metaExts = []
            , metaGhcOptions = []
            , metaExtraLibDirs = []
            , metaExtraIncludeDirs = []
            , metaPackages = []
            , metaSourceRepos = []
            , metaUnknownKeys = []
            }

{- | Parse a @source-repository-package@ value: whitespace-separated
@\<location\> \<ref\> [subdir]@ (not comma-separated like the other directives).
-}
parseSourceRepo :: Text -> Maybe SourceRepoPin
parseSourceRepo value = case T.words value of
    (loc : ref : rest) ->
        Just
            SourceRepoPin
                { srpLocation = loc
                , srpRef = ref
                , srpSubdir = case rest of
                    (sub : _) -> Just sub
                    [] -> Nothing
                }
    _ -> Nothing

parseCodeLine :: Text -> Line
parseCodeLine line
    | isBlankLine line = Blank
    | Just cmd <- parseGhciCommand line = GhciCommand cmd
    | Just pragma <- parsePragma line = Pragma pragma
    | Just imp <- parseImport line = Import imp
    | Just stripped <- stripTopLevelLet line = HaskellLine stripped
    | otherwise = HaskellLine (rewriteSplice line)

{- | Strip @let@ from top-level bindings.
  GHCi's @:{...}@ blocks don't support @let@ at the top level.
  Preserves @let ... in ...@ expressions and indented @let@ (inside do/where).
-}
stripTopLevelLet :: Text -> Maybe Text
stripTopLevelLet line = do
    rest <- T.stripPrefix "let " line
    if " in " `T.isInfixOf` rest
        then Nothing
        else Just rest

isBlankLine :: Text -> Bool
isBlankLine = T.null . T.strip

parseGhciCommand :: Text -> Maybe Text
parseGhciCommand line =
    let stripped = T.stripStart line
     in case T.uncons stripped of
            Just (':', rest) -> Just (":" <> rest)
            _ -> Nothing

parsePragma :: Text -> Maybe Text
parsePragma line =
    if "{-#" `T.isPrefixOf` line
        then Just line
        else Nothing

parseImport :: Text -> Maybe Text
parseImport line =
    case T.stripPrefix "import" line of
        Just rest
            | not (T.null rest) && (T.head rest == ' ' || T.head rest == '\t') ->
                Just line
        _ -> Nothing

-- https://discourse.haskell.org/t/injecting-variables-into-a-ghci-session/12558/2?u=mchav
rewriteSplice :: Text -> Text
rewriteSplice line =
    case T.stripPrefix "$(" line >>= T.stripSuffix ")" of
        Just inner -> "_ = (); " <> T.strip inner
        Nothing -> line
