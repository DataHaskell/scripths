module ScriptHs.Notebook where

import Data.Bifunctor (Bifunctor (second))
import Data.Bits (xor)
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word (Word32)
import Numeric (showHex)
import System.CPUTime (getCPUTime)

import ScriptHs.Markdown (
    CodeOutput (..),
    MimeType (..),
    Segment (..),
    parseMarkdown,
    reassemble,
 )
import ScriptHs.Parser (
    CabalMeta (..),
    Line (..),
    ScriptFile (..),
    mergeMetas,
    parseScript,
 )
import ScriptHs.Render (lineText)
import ScriptHs.Repl (scripthsIO, scrubInternalNames)
import ScriptHs.Run (RunOptions, runScriptCapture)
import ScriptHs.Version (TagStyle (NotebookTag), stampVersion)

type IndexedSegments = [(Int, Segment)]
type IndexedBlocks = [(Int, [Line])]

{- | Run a notebook. Output streamed to stdout is left as-is so pipelines stay
clean; output written to a file (@-o@ or @--in-place@) is stamped with the
current scripths version tag at the top.
-}
runNotebook :: RunOptions -> FilePath -> Maybe FilePath -> IO ()
runNotebook opts path outputPath = do
    contents <- stripBom <$> TIO.readFile path
    outputMd <- processNotebook opts path contents
    case outputPath of
        Nothing -> TIO.putStr outputMd
        Just output -> TIO.writeFile output (stampVersion NotebookTag outputMd)

-- | Drop a leading UTF-8 BOM so it never sits in front of the version tag.
stripBom :: Text -> Text
stripBom t = fromMaybe t (T.stripPrefix "\65279" t)

processNotebook :: RunOptions -> FilePath -> Text -> IO Text
processNotebook opts notebookPath contents = do
    let indexedSegments = zip [0 ..] (parseMarkdown contents)
        (metas, indexedCodeBlocks) = parseBlocks indexedSegments
    if null indexedCodeBlocks
        then pure contents
        else executeCodeCells opts notebookPath metas indexedSegments indexedCodeBlocks

executeCodeCells ::
    RunOptions ->
    FilePath ->
    CabalMeta ->
    IndexedSegments ->
    IndexedBlocks ->
    IO Text
executeCodeCells opts notebookPath meta allSegments codeBlocks = do
    let ghciScript0 = generatedMarkedScript "" codeBlocks
    nonce <- makeNonce ghciScript0
    let ghciScript = generatedMarkedScript nonce codeBlocks
        sf = ScriptFile{scriptMeta = meta, scriptCompile = Nothing, scriptLines = ghciScript}
    rawOutput <- runScriptCapture opts notebookPath sf
    let indices = map fst codeBlocks
        outputs =
            map
                (fmap (scrubCellOutput nonce indices))
                (splitByMarkers nonce rawOutput indices)
        blocksWithOutput = addOutputToSegments outputs allSegments
    pure $ reassemble blocksWithOutput

{- | A per-run hex nonce woven into every cell-end marker so a cell's own output
cannot spoof a marker and misattribute another cell's output. Mixes the process
CPU time with a hash of the cell text.
-}
makeNonce :: [Line] -> IO Text
makeNonce ls = do
    t <- getCPUTime
    -- TODO: mchavinda - if this is slow then drop the text hashing and just use time.
    let seed = T.pack (show t) <> T.concat (map lineText ls)
    pure (T.pack (showHex (fnv1a seed) ""))

fnv1a :: Text -> Word32
fnv1a = T.foldl' (\h c -> (h `xor` fromIntegral (ord c)) * 16777619) 2166136261

{- | Clean a cell's captured output before it is rendered into the document:
strip scripths' internal identifiers (so a diagnostic reads like vanilla GHCi)
and remove any cell-end block marker that survived 'splitByMarkers' (so it can
never appear interleaved with the cell's stdout/error).
-}
scrubCellOutput :: Text -> [Int] -> Text -> Text
scrubCellOutput nonce indices =
    scrubInternalNames . stripMarkers
  where
    stripMarkers t = foldr (\i acc -> T.replace (mkMarker nonce i) "" acc) t indices

addOutputToSegments :: [(Int, Text)] -> IndexedSegments -> [Segment]
addOutputToSegments outputs = map addOutput
  where
    addOutput :: (Int, Segment) -> Segment
    addOutput (i, CodeBlock lang code _) = CodeBlock lang code (fmap (CodeOutput MimePlain) (lookup i outputs))
    addOutput (_, seg) = seg

mkIndexedCodeSegments :: IndexedSegments -> IndexedSegments
mkIndexedCodeSegments segments = [(i, c) | (i, c@(CodeBlock lang _ _)) <- segments, isHaskell lang]

parseBlocks :: IndexedSegments -> (CabalMeta, IndexedBlocks)
parseBlocks blocks = (metas, map (second scriptLines) sfs)
  where
    sfs =
        [(i, parseScript code) | (i, CodeBlock lang code _) <- blocks, isHaskell lang]
    metas = mergeMetas (map (scriptMeta . snd) sfs)

generatedMarkedScript :: Text -> IndexedBlocks -> [Line]
generatedMarkedScript nonce = concatMap renderWithMarker
  where
    renderWithMarker :: (Int, [Line]) -> [Line]
    renderWithMarker (idx, ls) =
        ls
            ++ [ Blank
               , HaskellLine (markerStatement (mkMarker nonce idx))
               , Blank
               ]

{- | A GHCi statement printing a cell-end marker. @putStrLn@ is qualified through
the synthetic @System.IO@ alias ('scripthsIO') so the marker still prints under a
notebook that enables @NoImplicitPrelude@ or imports a custom prelude.
-}
markerStatement :: Text -> Text
markerStatement marker =
    scripthsIO <> ".putStrLn " <> T.pack (show (T.unpack marker))

splitByMarkers :: Text -> Text -> [Int] -> [(Int, Text)]
splitByMarkers _ _ [] = []
splitByMarkers nonce remaining (idx : rest) =
    let marker = mkMarker nonce idx
        (before, after) = T.breakOn marker remaining
     in if T.null after
            then [(idx, T.strip before)]
            else
                (idx, T.strip before)
                    : splitByMarkers nonce (T.drop (T.length marker) after) rest

{- | The marker printed to GHCi output to denote the end of a cell's execution.
Carries a per-run @nonce@ (see 'makeNonce') so a cell's own output cannot forge
a marker and steal the boundary between cells.
-}
mkMarker :: Text -> Int -> Text
mkMarker nonce n =
    "---SCRIPTHS_BLOCK_" <> nonce <> "_" <> T.pack (show n) <> "_END---"

isHaskell :: Text -> Bool
isHaskell lang = fenceLanguage lang `elem` ["haskell", "hs"]

isPython :: Text -> Bool
isPython lang = fenceLanguage lang `elem` ["python", "py"]

{- | The base language name of a code-fence info string, lower-cased and
trimmed. Handles both bare tags (@haskell@, @hs@) and Pandoc-style attribute
tags: @{haskell}@, @{.haskell}@, @{.haskell:hs}@, @{.haskell:ghci}@.
-}
fenceLanguage :: Text -> Text
fenceLanguage lang =
    let stripped = T.strip lang
        inner = case T.stripPrefix "{" stripped of
            Just s -> T.takeWhile (/= '}') s
            Nothing -> stripped
        token = case T.words inner of
            (t : _) -> t
            [] -> ""
        base = T.takeWhile (/= ':') (T.dropWhile (== '.') token)
     in T.toLower base
