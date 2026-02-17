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
    Line (..),
    parseScript,
) where

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
Right (ScriptFile {scriptMeta = CabalMeta {metaDeps = ["text"], metaExts = [], metaGhcOptions = []}, scriptLines = [Import "import Data.Text (Text)"]})

>>> parseScript ""
Right (ScriptFile {scriptMeta = CabalMeta {metaDeps = [], metaExts = [], metaGhcOptions = []}, scriptLines = []})
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
                _ -> emptyCabal
        _ -> Nothing
  where
    emptyCabal = CabalMeta{metaDeps = [], metaExts = [], metaGhcOptions = []}

parseCodeLine :: Text -> Line
parseCodeLine line
    | isBlankLine line = Blank
    | Just cmd <- parseGhciCommand line = GhciCommand cmd
    | Just pragma <- parsePragma line = Pragma pragma
    | Just imp <- parseImport line = Import imp
    | otherwise = HaskellLine (rewriteSplice line)

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
