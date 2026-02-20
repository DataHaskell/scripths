module ScriptHs.Notebook where

import Data.Bifunctor (Bifunctor (second))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import ScriptHs.Markdown (Segment (..), parseMarkdown, reassemble)
import ScriptHs.Parser (
    CabalMeta (..),
    Line (..),
    ScriptFile (..),
    mergeMetas,
    parseScript,
 )
import ScriptHs.Run (runScriptCapture)

type IndexedSegments = [(Int, Segment)]
type IndexedBlocks = [(Int, [Line])]

runNotebook :: FilePath -> Maybe FilePath -> IO ()
runNotebook path outputPath = do
    contents <- TIO.readFile path
    outputMd <- processNotebook contents
    case outputPath of
        Nothing -> TIO.putStr outputMd
        Just output -> TIO.writeFile output outputMd

processNotebook :: Text -> IO Text
processNotebook contents = do
    let indexedSegments = zip [0 ..] (parseMarkdown contents)
        (metas, indexedCodeBlocks) = parseBlocks indexedSegments
    if null indexedCodeBlocks
        then pure contents
        else executeCodeCells metas indexedSegments indexedCodeBlocks

executeCodeCells :: CabalMeta -> IndexedSegments -> IndexedBlocks -> IO Text
executeCodeCells meta allSegments codeBlocks = do
    let ghciScript = generatedMarkedScript codeBlocks
        sf = ScriptFile{scriptMeta = meta, scriptLines = ghciScript}
    rawOutput <- runScriptCapture sf
    let outputs = splitByMarkers rawOutput (map fst codeBlocks)
        blocksWithOutput = addOutputToSegments outputs allSegments
    pure $ reassemble blocksWithOutput

addOutputToSegments :: [(Int, Text)] -> IndexedSegments -> [Segment]
addOutputToSegments outputs = map addOutput
  where
    addOutput :: (Int, Segment) -> Segment
    addOutput (i, CodeBlock lang code _) = CodeBlock lang code (lookup i outputs)
    addOutput (_, seg) = seg

mkIndexedCodeSegments :: IndexedSegments -> IndexedSegments
mkIndexedCodeSegments segments = [(i, c) | (i, c@(CodeBlock lang _ _)) <- segments, isHaskell lang]

parseBlocks :: IndexedSegments -> (CabalMeta, IndexedBlocks)
parseBlocks blocks = (metas, map (second scriptLines) sfs)
  where
    sfs =
        [(i, parseScript code) | (i, CodeBlock lang code _) <- blocks, isHaskell lang]
    metas = mergeMetas (map (scriptMeta . snd) sfs)

generatedMarkedScript :: IndexedBlocks -> [Line]
generatedMarkedScript = concatMap renderWithMarker
  where
    renderWithMarker :: (Int, [Line]) -> [Line]
    renderWithMarker (idx, ls) =
        ls
            ++ [ Blank
               , HaskellLine ("putStrLn " <> T.pack (show (T.unpack (mkMarker idx))))
               , Blank
               ]

splitByMarkers :: Text -> [Int] -> [(Int, Text)]
splitByMarkers _ [] = []
splitByMarkers remaining (idx : rest) =
    let marker = mkMarker idx
        (before, after) = T.breakOn marker remaining
     in if T.null after
            then [(idx, T.strip before)]
            else
                (idx, T.strip before) : splitByMarkers (T.drop (T.length marker) after) rest

-- A marker we'll print to GHCi output to
-- denote the end of a cell execution block.
mkMarker :: Int -> Text
mkMarker n = "---SCRIPTHS_BLOCK_" <> T.pack (show n) <> "_END---"

isHaskell :: Text -> Bool
isHaskell lang = T.toLower (T.strip lang) `elem` ["haskell", "hs"]
