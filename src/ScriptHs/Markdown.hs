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
parseMarkdown' acc (line : rest) = case T.strip <$> T.stripPrefix fence line of
    Nothing -> parseMarkdown' (acc ++ [line]) rest
    Just lang ->
        let
            prose = T.unlines acc
            (codeLines, rest') = fmap (drop 1) (break ((== fence) . T.strip) rest)
            (output, rest'') = case dropWhile ((== "") . T.strip) rest' of
                (x : xs) ->
                    if not (T.isPrefixOf "> <!-- sabela:mime" x)
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

fenceCodeSegment :: Text -> Text -> Text
fenceCodeSegment lang output
    | T.null (T.strip output) = ""
    | otherwise = T.unlines ["", fence <> lang, T.stripEnd output, fence, ""]

reassemble :: [Segment] -> Text
reassemble = T.concat . map renderSegment

renderSegment :: Segment -> Text
renderSegment (Prose t) = t
renderSegment (CodeBlock lang code Nothing) = fenceCodeSegment lang code
renderSegment (CodeBlock lang code (Just output)) = fenceCodeSegment lang code <> blockQuote output

blockQuote :: CodeOutput -> Text
blockQuote (CodeOutput mimeType t) =
    let
        ls = ("<!-- sabela:mime " <> mimeIndicator mimeType <> " -->") : T.lines t
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
        MimeImage (T.takeWhile (/= ';') (T.drop (T.length "<!-- sabela:mime ") t))
    | otherwise = MimePlain
