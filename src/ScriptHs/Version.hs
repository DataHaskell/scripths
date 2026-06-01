{-# LANGUAGE OverloadedStrings #-}

{- | The scripths version and the first-line /version tag/ that marks a script or
notebook with the scripths version it was written for.

The tag lets scripths warn when a file was authored by a newer release than the
binary running it (so a user knows parsing may be incomplete). It is spelled the
way each file type's comments are spelled:

@
-- scripths: 0.4.1.0          (.ghci \/ .hs)
\<!-- scripths: 0.4.1.0 --\>    (.md \/ .markdown)
@

The emitted version is always 'scripthsVersion', a PVP value (dot-separated
decimals), so the notebook tag's comment text can never contain @--@ or a
trailing @-@ — it is a well-formed HTML comment by construction.
-}
module ScriptHs.Version (
    scripthsVersion,
    scripthsVersionText,
    TagStyle (..),
    tagStyleFor,
    versionTag,
    parseTagLine,
    tagVersion,
    stampVersion,
    newerVersionWarning,
) where

import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (Version, makeVersion, showVersion)
import System.FilePath (takeExtension)
import Text.Read (readMaybe)

import Paths_scripths (version)

-- | The running scripths version (single source of truth: the @.cabal@ @version@).
scripthsVersion :: Version
scripthsVersion = version

scripthsVersionText :: Text
scripthsVersionText = T.pack (showVersion scripthsVersion)

-- | How the version tag is spelled in a given file type.
data TagStyle
    = -- | @-- scripths: X@, for @.ghci@\/@.hs@ scripts.
      ScriptTag
    | -- | @\<!-- scripths: X --\>@, for @.md@\/@.markdown@ notebooks.
      NotebookTag
    deriving (Show, Eq)

-- | The tag style for a path, by extension (@.md@\/@.markdown@ are notebooks).
tagStyleFor :: FilePath -> TagStyle
tagStyleFor path
    | ext `elem` [".md", ".markdown"] = NotebookTag
    | otherwise = ScriptTag
  where
    ext = T.unpack (T.toLower (T.pack (takeExtension path)))

-- | The tag line for the current version, in the given style.
versionTag :: TagStyle -> Text
versionTag ScriptTag = "-- scripths: " <> scripthsVersionText
versionTag NotebookTag = "<!-- scripths: " <> scripthsVersionText <> " -->"

{- | The scripths version a single line declares, if it is a version tag of the
given style. Tolerant of a UTF-8 BOM, a trailing @\\r@ (CRLF), trailing
whitespace, and up to three leading spaces. The notebook output marker
@\<!-- scripths:mime … --\>@ is /not/ a version tag (its post-colon token is
@mime@, not a version), so it parses to 'Nothing'.
-}
parseTagLine :: TagStyle -> Text -> Maybe Version
parseTagLine style line = do
    let trimmed = normalizeLine line
    inner <- case style of
        ScriptTag -> T.stripPrefix "-- scripths:" trimmed
        NotebookTag ->
            T.stripPrefix "<!-- scripths:" trimmed
                >>= fmap T.stripEnd . T.stripSuffix "-->" . T.stripEnd
    -- Require whitespace after the colon, so @scripths:mime@ never matches.
    rest <- requireLeadingSpace inner
    parseVersionToken rest

-- | Strip a leading BOM, a trailing @\\r@\/whitespace, and up to three leading spaces.
normalizeLine :: Text -> Text
normalizeLine =
    dropLeadingSpaces (3 :: Int) . T.stripEnd . T.dropWhileEnd (== '\r') . dropBom
  where
    dropBom t = fromMaybe t (T.stripPrefix "\65279" t)
    dropLeadingSpaces 0 t = t
    dropLeadingSpaces n t = case T.uncons t of
        Just (' ', r) -> dropLeadingSpaces (n - 1) r
        _ -> t

requireLeadingSpace :: Text -> Maybe Text
requireLeadingSpace t = case T.uncons t of
    Just (c, _) | isSpace c -> Just (T.stripStart t)
    _ -> Nothing

-- | Parse a leading dotted-decimal version (e.g. @0.4.1.0@), ignoring trailing text.
parseVersionToken :: Text -> Maybe Version
parseVersionToken t =
    case T.splitOn "." token of
        parts@(_ : _)
            | not (any T.null parts) ->
                makeVersion <$> traverse (readMaybe . T.unpack) parts
        _ -> Nothing
  where
    token = T.takeWhile (\c -> c == '.' || isDigit c) (T.stripStart t)

{- | The scripths version a file declares, if any. Scans a small top-of-file
window: skip a leading @#!@ shebang (scripts) or a leading @---@ YAML
frontmatter block (notebooks), then read the first non-blank line.
-}
tagVersion :: TagStyle -> Text -> Maybe Version
tagVersion style contents =
    case dropWhile isBlank (snd (splitPreamble style (T.lines contents))) of
        (l : _) -> parseTagLine style l
        [] -> Nothing
  where
    isBlank = T.null . T.strip

{- | Split off a leading preamble that the tag sits /after/: a @#!@ shebang for
scripts, or a @---@…@---@ YAML frontmatter block for notebooks. Returns
@(preamble, rest)@.
-}
splitPreamble :: TagStyle -> [Text] -> ([Text], [Text])
splitPreamble ScriptTag ls@(l : rest)
    | "#!" `T.isPrefixOf` T.stripStart l = ([l], rest)
    | otherwise = ([], ls)
splitPreamble NotebookTag ls@(l : rest)
    | T.strip l == "---" =
        let (block, after) = break ((== "---") . T.strip) rest
         in case after of
                (closing : afterClosing) -> (l : block ++ [closing], afterClosing)
                [] -> ([], ls)
    | otherwise = ([], ls)
splitPreamble _ [] = ([], [])

{- | Ensure @contents@ carries the current version tag at the top of its body:
replace an existing tag line in place, or insert one after any shebang\/
frontmatter preamble. Exactly one blank line separates the tag from the body.
Idempotent.
-}
stampVersion :: TagStyle -> Text -> Text
stampVersion style contents =
    rejoin (preamble ++ tag : separator ++ body)
  where
    tag = versionTag style
    (preamble, rest) = splitPreamble style (T.lines contents)
    body = dropWhile isBlankLine (dropExistingTag rest)
    separator = ["" | not (null body)]
    dropExistingTag ls =
        let (_blanks, rest') = span isBlankLine ls
         in case rest' of
                (l : more) | isJustTag l -> more
                _ -> ls
    isJustTag l = isJust (parseTagLine style l)
    isBlankLine = T.null . T.strip

{- | Re-join lines, trimming leading\/trailing blank lines and re-adding a single
trailing newline iff there is content ('T.lines' drops the final newline).
-}
rejoin :: [Text] -> Text
rejoin ls =
    let trimmed = reverse (dropBlank (reverse (dropBlank ls)))
     in if null trimmed then "" else T.intercalate "\n" trimmed <> "\n"
  where
    dropBlank = dropWhile (T.null . T.strip)

-- | @Just@ a warning iff @declared@ is newer than the running scripths.
newerVersionWarning :: Version -> Maybe Text
newerVersionWarning declared
    | declared > scripthsVersion =
        Just $
            "file declares scripths "
                <> T.pack (showVersion declared)
                <> " but you are running "
                <> scripthsVersionText
                <> "; parsing may be incomplete \8212 consider upgrading scripths"
    | otherwise = Nothing
