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
    SourceRepoPin (..),
    Line (..),
    parseScript,
    mergeMetas,
) where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

{- | A fully parsed script, consisting of aggregated cabal metadata and an
ordered list of code lines.
-}
data ScriptFile = ScriptFile
    { scriptMeta :: CabalMeta
    -- ^ Aggregated metadata from all @-- cabal:@ directives in the script.
    , scriptLines :: [Line]
    -- ^ The code lines of the script, in order, with metadata lines removed.
    }
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
ScriptFile {scriptMeta = CabalMeta {metaDeps = ["text"], metaExts = [], metaGhcOptions = [], metaSourceRepos = [], metaUnknownKeys = []}, scriptLines = [Import "import Data.Text (Text)"]}

>>> parseScript ""
ScriptFile {scriptMeta = CabalMeta {metaDeps = [], metaExts = [], metaGhcOptions = [], metaSourceRepos = [], metaUnknownKeys = []}, scriptLines = []}
-}
parseScript :: Text -> ScriptFile
parseScript input =
    let textLines = T.lines input
        parsedLines = map parseLine textLines
        (metas, code) = partitionLines parsedLines
        meta = mergeMetas metas
     in ScriptFile{scriptMeta = meta, scriptLines = code}

data RawLine
    = RawCabalMeta CabalMeta
    | RawCode Line

partitionLines :: [RawLine] -> ([CabalMeta], [Line])
partitionLines = foldr go ([], [])
  where
    go (RawCabalMeta m) (ms, cs) = (m : ms, cs)
    go (RawCode c) (ms, cs) = (ms, c : cs)

mergeMetas :: [CabalMeta] -> CabalMeta
mergeMetas ms =
    CabalMeta
        { metaDeps = concatMap metaDeps ms
        , metaExts = concatMap metaExts ms
        , metaGhcOptions = concatMap metaGhcOptions ms
        , metaSourceRepos = concatMap metaSourceRepos ms
        , metaUnknownKeys = concatMap metaUnknownKeys ms
        }

parseLine :: Text -> RawLine
parseLine line
    | Just meta <- parseCabalMeta line = RawCabalMeta meta
    | otherwise = RawCode (parseCodeLine line)

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
