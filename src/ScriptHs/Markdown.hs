module ScriptHs.Markdown (
    Segment (..),
    parseMarkdown,
    reassemble,
    reassembleWith,
    MimeType (..),
    CodeOutput (..),
    CodeStyle (..),
    parseCodeStyle,
    defaultCodeStyle,
    OutputStyle (..),
    parseOutputStyle,
    defaultOutputStyle,
    RenderOptions (..),
    defaultRenderOptions,
) where

import Data.Text (Text)
import qualified Data.Text as T

{- |  Directs how the original code fences are styled
  in the processed output.
-}
data CodeStyle
    = DisplayCode
    | RemoveCode
    deriving (Eq, Ord, Show)

parseCodeStyle :: String -> Maybe CodeStyle
parseCodeStyle "display" = Just DisplayCode
parseCodeStyle "remove" = Just RemoveCode
parseCodeStyle _ = Nothing

defaultCodeStyle :: CodeStyle
defaultCodeStyle = DisplayCode

data OutputStyle
    = OutputQuoted
    | OutputRaw
    deriving (Eq, Ord, Show)

parseOutputStyle :: String -> Maybe OutputStyle
parseOutputStyle "quoted" = Just OutputQuoted
parseOutputStyle "raw" = Just OutputRaw
parseOutputStyle _ = Nothing

defaultOutputStyle :: OutputStyle
defaultOutputStyle = OutputQuoted

{- | Render-time presentation choices; kept out of the parsed 'CodeOutput'
content model. Consumed only by 'reassembleWith'.
-}
data RenderOptions = RenderOptions
    { renderCodeStyle :: CodeStyle
    , renderOutputStyle :: OutputStyle
    }
    deriving (Eq, Show)

defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions defaultCodeStyle defaultOutputStyle

data MimeType
    = MimeHtml
    | MimeMarkdown
    | MimeSvg
    | MimeLatex
    | MimeJson
    | MimeImage Text -- image type
    | MimePlain
    deriving (Show, Eq)

data CodeOutput = CodeOutput MimeType Text deriving (Show, Eq)

data Segment
    = Prose Text
    | CodeBlock Text Text (Maybe CodeOutput) -- Language, Code, Output
    deriving (Show, Eq)

parseMarkdown :: Text -> [Segment]
parseMarkdown = parseMarkdown' [] . T.lines

parseMarkdown' :: [Text] -> [Text] -> [Segment]
parseMarkdown' acc [] = [Prose prose | not (isBlank prose)]
  where
    prose = T.unlines acc
parseMarkdown' acc (line : rest) = case fenceLang line of
    Nothing -> parseMarkdown' (acc ++ [line]) rest
    Just lang ->
        let
            prose = T.unlines acc
            (codeLines, rest') = fmap (drop 1) (break ((== fence) . T.strip) rest)
            (output, rest'') = case dropWhile ((== "") . T.strip) rest' of
                (x : xs)
                    | isMimeMarkerLine x ->
                        let (body, afterOutput) = extractOutput x xs
                         in (Just (CodeOutput (mimeFromTag x) body), afterOutput)
                _ -> (Nothing, rest')
            segments =
                [Prose prose | not (isBlank prose)]
                    ++ [CodeBlock lang (T.unlines codeLines) output]
         in
            segments ++ parseMarkdown' [] rest''

{- | A prose run that is empty or only whitespace carries no cell content;
the blank line(s) between two code fences parse to such a run and must not
become a spurious prose segment.
-}
isBlank :: Text -> Bool
isBlank = T.null . T.strip

fence :: Text
fence = "```"

{- | The language tag of a code-fence /opener/, or 'Nothing' if the line is not
one. A fence is /exactly/ three backticks (optionally followed by a language
tag) — a line of four or more backticks is not a fence and stays prose.
-}
fenceLang :: Text -> Maybe Text
fenceLang line = do
    rest <- T.stripPrefix fence line
    if "`" `T.isPrefixOf` rest then Nothing else Just (T.strip rest)

{- | The HTML-comment marker that tags a code block's rendered output with its
MIME type. We render the @scripths:mime@ form; for backward compatibility we
still recognise the legacy @sabela:mime@ marker when re-parsing older notebooks.
-}
mimeMarker :: Text
mimeMarker = "<!-- scripths:mime "

{- | Does this line open a rendered-output block? Matches either marker
spelling, quoted (@> @) or raw.
-}
isMimeMarkerLine :: Text -> Bool
isMimeMarkerLine x =
    T.isPrefixOf "<!-- scripths:mime" bare || T.isPrefixOf "<!-- sabela:mime" bare
  where
    bare = if T.isPrefixOf "> " x then T.drop 2 x else x

{- | Closing marker that terminates a raw output block so it re-parses; quoted
blocks self-delimit via their @> @ prefix and need no terminator.
-}
endMarker :: Text
endMarker = "<!-- /scripths:mime -->"

isEndMarkerLine :: Text -> Bool
isEndMarkerLine l = T.strip l == endMarker

{- | Collect an output block's body and the lines after it: a quoted block
runs while lines keep the @> @ prefix; a raw block runs to 'endMarker'.
-}
extractOutput :: Text -> [Text] -> (Text, [Text])
extractOutput marker xs
    | T.isPrefixOf "> " marker =
        let (body, after) = span (T.isPrefixOf "> ") xs
         in (T.unlines (map (T.drop (T.length "> ")) body), after)
    | otherwise =
        let (body, after) = break isEndMarkerLine xs
         in (T.unlines body, drop 1 after)

fenceCodeSegment :: Text -> Text -> Text
fenceCodeSegment lang output
    | T.null (T.strip output) = ""
    | otherwise = T.unlines ["", fence <> lang, T.stripEnd output, fence, ""]

-- | 'reassembleWith' under 'defaultRenderOptions'; the back-compatible default.
reassemble :: [Segment] -> Text
reassemble = reassembleWith defaultRenderOptions

{- | Render segments back to markdown idempotently (a @--in-place@ re-run is a
fixed point): seam blank runs collapse to one and leading/trailing blanks are
trimmed. Output re-parses under every style — 'OutputRaw' is bounded by a
closing 'endMarker', and 'RemoveCode' leaves no fence so a re-run is a no-op.
-}
reassembleWith :: RenderOptions -> [Segment] -> Text
reassembleWith opts = finalize . foldr (joinSeam . renderSegment opts) ""
  where
    joinSeam "" acc = acc
    joinSeam piece "" = piece
    joinSeam piece acc =
        T.dropWhileEnd (== '\n') piece
            <> T.replicate (min 2 (trailingNL piece + leadingNL acc)) "\n"
            <> T.dropWhile (== '\n') acc
    trailingNL = T.length . T.takeWhileEnd (== '\n')
    leadingNL = T.length . T.takeWhile (== '\n')
    finalize t =
        let stripped = T.dropWhileEnd (== '\n') (T.dropWhile (== '\n') t)
         in if T.null stripped then "" else stripped <> "\n"

renderSegment :: RenderOptions -> Segment -> Text
renderSegment _ (Prose t) = t
renderSegment _ (CodeBlock lang code Nothing) = fenceCodeSegment lang code
renderSegment opts (CodeBlock lang code (Just output)) =
    let codeFence = case renderCodeStyle opts of
            DisplayCode -> fenceCodeSegment lang code
            RemoveCode -> mempty
     in codeFence <> blockQuote (renderOutputStyle opts) output

blockQuote :: OutputStyle -> CodeOutput -> Text
blockQuote outputStyle (CodeOutput mimeType t) =
    case outputStyle of
        OutputQuoted -> T.unlines (map quote (marker : body)) <> "\n"
        OutputRaw -> T.unlines (marker : body ++ [endMarker]) <> "\n"
  where
    marker = mimeMarker <> mimeIndicator mimeType <> " -->"
    body = reverse $ dropWhile T.null $ reverse (T.lines t)
    quote l = if T.null l then "> " else "> " <> l

mimeIndicator :: MimeType -> Text
mimeIndicator m = case m of
    MimeHtml -> "text/html"
    MimeMarkdown -> "text/markdown"
    MimeSvg -> "image/svg+xml"
    MimeLatex -> "text/latex"
    MimeJson -> "application/json"
    MimeImage t -> t <> ";base64"
    MimePlain -> "text/plain"

mimeFromTag :: Text -> MimeType
mimeFromTag t
    | T.isInfixOf "text/html" t = MimeHtml
    | T.isInfixOf "text/markdown" t = MimeMarkdown
    | T.isInfixOf "image/svg+xml" t = MimeSvg
    | T.isInfixOf "text/latex" t = MimeLatex
    | T.isInfixOf "application/json" t = MimeJson
    | T.isInfixOf "base64" t =
        -- The part after "mime " up to the ";base64", regardless of marker name.
        MimeImage (T.strip (T.takeWhile (/= ';') (snd (T.breakOnEnd "mime " t))))
    | otherwise = MimePlain
