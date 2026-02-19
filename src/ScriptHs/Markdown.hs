module ScriptHs.Markdown (
    Segment (..),
    parseMarkdown,
    reassemble,
) where

import Data.Text (Text)
import qualified Data.Text as T

data Segment
    = Prose Text
    | CodeBlock Text Text (Maybe Text)
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
            segments =
                [Prose prose | not (T.null prose)]
                    ++ [CodeBlock lang (T.unlines codeLines) Nothing]
         in
            segments ++ parseMarkdown' [] rest'

fence :: Text
fence = "```"

fenceCodeSegment :: Text -> Text -> Text
fenceCodeSegment lang output = "\n" <> fence <> lang <> "\n" <> output <> fence <> "\n"

reassemble :: [Segment] -> Text
reassemble = T.concat . map renderSegment

renderSegment :: Segment -> Text
renderSegment (Prose t) = t
renderSegment (CodeBlock lang code Nothing) = fenceCodeSegment lang code
renderSegment (CodeBlock lang code (Just output)) =
    if T.null (T.strip output)
        then fenceCodeSegment lang code
        else fenceCodeSegment lang code <> blockQuote output

blockQuote :: Text -> Text
blockQuote t =
    let
        ls = T.lines t
        trimmed = reverse $ dropWhile T.null $ reverse ls
        quoted = T.unlines $ map (\l -> if T.null l then ">" else "> " <> l) trimmed
     in
        quoted <> "\n"
