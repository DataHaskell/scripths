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

import Control.Applicative (many, (<|>))
import Control.Monad (void, when)
import Data.Attoparsec.Text (
    Parser,
    atEnd,
    char,
    endOfInput,
    isHorizontalSpace,
    parseOnly,
    satisfy,
    skipWhile,
    string,
    takeWhile,
    takeWhile1,
    (<?>),
 )
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (takeWhile)

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
parseScript :: Text -> Either String ScriptFile
parseScript = parseOnly scriptFileP

scriptFileP :: Parser ScriptFile
scriptFileP = do
    ls <- many lineP <* endOfInput
    let (metas, code) = partitionLines ls
        meta = mergeMetas metas
    pure (ScriptFile{scriptMeta = meta, scriptLines = code})

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

lineP :: Parser RawLine
lineP = do
    end <- atEnd
    when end (fail "unexpected end of input")
    (RawCabalMeta <$> cabalMetaP <|> RawCode <$> codeLineP)
        <?> "line"

cabalMetaP :: Parser CabalMeta
cabalMetaP = do
    _ <- string "-- cabal:"
    skipWhile isHorizontalSpace
    key <- takeWhile1 (/= ':')
    _ <- char ':'
    skipWhile isHorizontalSpace
    values <- restOfLine
    let items = map T.strip (T.splitOn "," values)
    pure $ case T.strip key of
        "build-depends" -> emptyCabal{metaDeps = items}
        "default-extensions" -> emptyCabal{metaExts = items}
        "ghc-options" -> emptyCabal{metaGhcOptions = items}
        _ -> emptyCabal
  where
    emptyCabal = CabalMeta{metaDeps = [], metaExts = [], metaGhcOptions = []}

codeLineP :: Parser Line
codeLineP =
    Blank <$ blankLineP
        <|> GhciCommand <$> ghciCommandP
        <|> Pragma <$> pragmaP
        <|> Import <$> importP
        <|> HaskellLine <$> restOfLine

blankLineP :: Parser ()
blankLineP = skipWhile isHorizontalSpace *> void (char '\n')

-- TODO: mchavinda - This will parse multiple braces
-- as GHCi directives. It's a rare corner case but we
-- fix it in the future.
ghciCommandP :: Parser Text
ghciCommandP = do
    skipWhile isHorizontalSpace
    _ <- char ':'
    rest <- restOfLine
    pure (":" <> rest)

pragmaP :: Parser Text
pragmaP = do
    s <- string "{-#"
    rest <- restOfLine
    pure (s <> rest)

importP :: Parser Text
importP = do
    s <- string "import"
    c <- satisfy (\ch -> ch == ' ' || ch == '\t')
    rest <- restOfLine
    pure (s <> T.singleton c <> rest)

restOfLine :: Parser Text
restOfLine = do
    t <- takeWhile (/= '\n')
    void (char '\n') <|> endOfInput
    -- https://discourse.haskell.org/t/injecting-variables-into-a-ghci-session/12558/2?u=mchav
    case T.stripPrefix "$(" t >>= T.stripSuffix ")" of
        Just inner -> pure $ "_ = (); " <> T.strip inner
        _ -> pure t
