module ScriptHs.Markdown (
    Segment (..),
    parseMarkdown,
    reassemble,
    MimeType (..),
    CodeOutput (..),
) where

import Data.Text (Text)
import qualified Data.Text as T

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
parseMarkdown' acc [] = [Prose prose | not (T.null prose)]
  where
    prose = T.unlines acc
parseMarkdown' acc (line : rest) = case fenceLang line of
    Nothing -> parseMarkdown' (acc ++ [line]) rest
    Just lang ->
        let
            prose = T.unlines acc
            (codeLines, rest') = fmap (drop 1) (break ((== fence) . T.strip) rest)
            (output, rest'') = case dropWhile ((== "") . T.strip) rest' of
                (x : xs) ->
                    if not (isMimeMarkerLine x)
                        then (Nothing, rest')
                        else
                            let
                                (cOutput, afterOutput) = span (T.isPrefixOf "> ") xs
                                mType = mimeFromTag x
                             in
                                ( Just (CodeOutput mType (T.unlines (map (T.drop (T.length "> ")) cOutput)))
                                , afterOutput
                                )
                [] -> (Nothing, rest')
            segments =
                [Prose prose | not (T.null prose)]
                    ++ [CodeBlock lang (T.unlines codeLines) output]
         in
            segments ++ parseMarkdown' [] rest''

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

-- | Does this line open a rendered-output block (either marker spelling)?
isMimeMarkerLine :: Text -> Bool
isMimeMarkerLine x =
    T.isPrefixOf "> <!-- scripths:mime" x || T.isPrefixOf "> <!-- sabela:mime" x

fenceCodeSegment :: Text -> Text -> Text
fenceCodeSegment lang output
    | T.null (T.strip output) = ""
    | otherwise = T.unlines ["", fence <> lang, T.stripEnd output, fence, ""]

{- | Render segments back to markdown idempotently: the blank-line run at each
seam between segments collapses to one blank line and the document's leading/
trailing blanks are trimmed, so re-running (e.g. @--in-place@) adds no new lines.
-}
reassemble :: [Segment] -> Text
reassemble = finalize . foldr (joinSeam . renderSegment) ""
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

renderSegment :: Segment -> Text
renderSegment (Prose t) = t
renderSegment (CodeBlock lang code Nothing) = fenceCodeSegment lang code
renderSegment (CodeBlock lang code (Just output)) = fenceCodeSegment lang code <> blockQuote output

blockQuote :: CodeOutput -> Text
blockQuote (CodeOutput mimeType t) =
    let
        ls = (mimeMarker <> mimeIndicator mimeType <> " -->") : T.lines t
        trimmed = reverse $ dropWhile T.null $ reverse ls
        quoted = T.unlines $ map (\l -> if T.null l then "> " else "> " <> l) trimmed
     in
        quoted <> "\n"

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
