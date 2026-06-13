{-# LANGUAGE OverloadedStrings #-}

{- | Rendering compiled notebook cells into a generated Haskell module.

A cell carrying a @-- compile@ directive contributes its declarations to a
module that the host loads with @:add@, so GHC compiles it to native object
code instead of interpreting it at the prompt. Every import and declaration
unit is preceded by a @{\-# LINE #-\}@ pragma whose \"file\" is a per-cell tag
('linePragmaTag'), so GHC diagnostics come back cell-relative.
-}
module ScriptHs.Compiled (
    CellChunk (..),
    CompileIssue (..),
    checkCompilable,
    renderCompiledModule,
    linePragmaTag,
    parseLinePragmaTag,
    isValidModuleName,
) where

import Data.Char (isAlphaNum, isAsciiUpper)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

import ScriptHs.Parser (Line (..))
import ScriptHs.Render (
    Kind (..),
    Piece (..),
    lineText,
    toPieces,
    unRewriteSplice,
 )

-- | One cell's contribution to a generated module.
data CellChunk = CellChunk
    { ccTag :: Text
    -- ^ LINE-pragma file tag for this cell (see 'linePragmaTag').
    , ccLines :: [(Int, Line)]
    -- ^ The cell's code lines with their 1-based cell-relative numbers.
    }
    deriving (Show, Eq)

-- | Why a cell cannot be compiled, pointing at the offending line.
data CompileIssue = CompileIssue
    { ciLine :: Int
    , ciReason :: Text
    }
    deriving (Show, Eq)

{- | Validate that a cell contains only module-top-level content: imports,
pragmas, declarations, TH splices, comments, and @:set -X@ directives.
Expressions, monadic binds, and other GHCi commands are rejected.
-}
checkCompilable :: [(Int, Line)] -> [CompileIssue]
checkCompilable nls =
    [ CompileIssue i reason
    | (i, p) <- numberedPieces nls
    , Just reason <- [pieceIssue p]
    ]

pieceIssue :: Piece -> Maybe Text
pieceIssue PBlank = Nothing
pieceIssue (PPragma _) = Nothing
pieceIssue (PImport _) = Nothing
pieceIssue (PGhciCommand t)
    | isSetExtension t = Nothing
    | otherwise =
        Just "GHCi commands are not allowed in compiled cells (only :set -XExtension)"
pieceIssue (PUnit k _) = case k of
    KComment -> Nothing
    KDeclaration -> Nothing
    KTHSplice -> Nothing
    KAction ->
        Just
            "compiled cells contain only declarations — move this expression to an interpreted cell below, or remove '-- compile'"
    KIOBind ->
        Just
            "monadic binds (x <- …) are not allowed in compiled cells — move this to an interpreted cell below, or remove '-- compile'"

{- | Pair each piece from 'toPieces' with the original line number of its
first line. Relies on 'toPieces' being line-count preserving: every piece
consumes exactly the lines it embeds.
-}
numberedPieces :: [(Int, Line)] -> [(Int, Piece)]
numberedPieces nls = go nls (toPieces (map snd nls))
  where
    go _ [] = []
    go rest (p : ps) = case rest of
        ((i, _) : _) -> (i, p) : go (drop (pieceLen p) rest) ps
        [] -> []
    pieceLen (PUnit _ ls) = length ls
    pieceLen _ = 1

-- | Extensions named by a @:set -X@\/@:seti -X@ line whose args are all @-X@.
setExtensions :: Text -> [Text]
setExtensions t = case T.words (T.strip t) of
    (cmd : rest)
        | cmd `elem` [":set", ":seti"]
        , exts <- [e | w <- rest, Just e <- [T.stripPrefix "-X" w]]
        , length exts == length rest ->
            exts
    _ -> []

isSetExtension :: Text -> Bool
isSetExtension = not . null . setExtensions

-- | The LINE-pragma \"file\" tag for a cell id, e.g. @sabela-cell-12@.
linePragmaTag :: Int -> Text
linePragmaTag cid = "sabela-cell-" <> T.pack (show cid)

-- | Inverse of 'linePragmaTag'; the error parser uses this to route diagnostics.
parseLinePragmaTag :: Text -> Maybe Int
parseLinePragmaTag t = do
    rest <- T.stripPrefix "sabela-cell-" t
    case TR.decimal rest of
        Right (n, leftover) | T.null leftover -> Just n
        _ -> Nothing

-- | Valid Haskell module name: dot-separated capitalized identifier segments.
isValidModuleName :: Text -> Bool
isValidModuleName name =
    not (T.null name) && all validSegment (T.splitOn "." name)
  where
    validSegment seg = case T.uncons seg of
        Just (c, rest) -> isAsciiUpper c && T.all segChar rest
        Nothing -> False
    segChar c = isAlphaNum c || c == '_' || c == '\''

{- | Assemble a generated module: deduped LANGUAGE pragmas (defaults, cell
pragmas, @:set -X@ contributions), the module header, synthetic imports (e.g.
cross-module dependencies the host inferred), cell imports and declarations —
each cell unit preceded by a LINE pragma pointing back at its source cell.
-}
renderCompiledModule ::
    -- | Module name
    Text ->
    -- | Default extensions (rendered as LANGUAGE pragmas)
    [Text] ->
    -- | Synthetic imports, verbatim (e.g. @import OtherModule@)
    [Text] ->
    [CellChunk] ->
    Text
renderCompiledModule modName defaultExts extraImports chunks =
    T.unlines . intercalate [""] . filter (not . null) $
        [ dedup (map languagePragma defaultExts ++ chunkPragmas)
            ++ ["{-# OPTIONS_GHC -Wno-unused-imports #-}"]
        , ["-- Generated by Sabela compile mode. Do not edit."]
        , ["module " <> modName <> " where"]
        , extraImports ++ concatMap chunkImports chunks
        , intercalate [""] (concatMap chunkDecls chunks)
        ]
  where
    chunkPragmas = concatMap pragmasOf chunks
    pragmasOf c =
        concat
            [ case p of
                PPragma t -> [t]
                PGhciCommand t -> map languagePragma (setExtensions t)
                _ -> []
            | (_, p) <- numberedPieces (ccLines c)
            ]
    chunkImports c =
        concat
            [ [linePragma i (ccTag c), t]
            | (i, PImport t) <- numberedPieces (ccLines c)
            ]
    chunkDecls c =
        [ linePragma i (ccTag c) : declLines u
        | (i, PUnit k u) <- numberedPieces (ccLines c)
        , k `elem` [KComment, KDeclaration, KTHSplice]
        ]
    declLines = T.lines . unRewriteSplice . T.intercalate "\n" . map lineText

linePragma :: Int -> Text -> Text
linePragma n tag = "{-# LINE " <> T.pack (show n) <> " \"" <> tag <> "\" #-}"

languagePragma :: Text -> Text
languagePragma ext = "{-# LANGUAGE " <> ext <> " #-}"

dedup :: [Text] -> [Text]
dedup = go []
  where
    go _ [] = []
    go seen (x : xs)
        | x `elem` seen = go seen xs
        | otherwise = x : go (x : seen) xs
