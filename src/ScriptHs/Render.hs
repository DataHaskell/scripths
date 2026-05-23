{-# LANGUAGE OverloadedStrings #-}

{- | Rendering 'ScriptHs.Parser.Line' sequences into GHCi-compatible scripts.

GHCi imposes constraints that raw Haskell source does not: multi-line
definitions must be wrapped in @:{@ \/ @:}@ blocks, and each
expression-statement or monadic bind (@<-@) must be a separate GHCi
statement. 'toGhciScript' groups input lines into logical units
(a lead non-indented line plus its indented continuations), classifies
each unit, then assembles blocks accordingly.
-}
module ScriptHs.Render (
    toGhciScript,

    -- * Module rendering (notebook → standalone Haskell)
    ModuleParts (..),
    TrailKind (..),
    TrailingResolver,
    toModule,
    actionExprs,
    renderModuleText,
    renderCabalScriptHeader,
    renderCabalScript,
    LhsBlock (..),
    renderLiterate,

    -- * Reusable line classification (for downstream tooling)
    Kind (..),
    Piece (..),
    toPieces,
    classify,
    lineText,
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import ScriptHs.Parser (CabalMeta (..), Line (..))

data Block
    = SingleLine Line
    | MultiLine [Line]
    deriving (Show, Eq)

{- | Render a list of 'Line's as a GHCi script.

Classification rules:

* A run of @--@ comment lines forms a 'KComment' unit that attaches
  forward to the next non-comment unit.
* A type signature, value binding, or clause-head-with-guards forms a
  'KDeclaration' unit; consecutive declarations merge into a single
  @:{ … :}@ block.
* A monadic bind (@<-@ on the lead line) forms a 'KIOBind' unit; each
  such unit is its own block.
* A Template Haskell splice (@$(…)@ or the rewritten @_ = (); …@ form)
  forms a 'KTHSplice' unit; each is its own block.
* Anything else is a 'KAction' (expression-statement, @do@-block, etc.);
  each action unit becomes its own block.
-}
toGhciScript :: [Line] -> Text
toGhciScript = T.unlines . concatMap renderBlock . piecesToBlocks . toPieces

---------------------------------------------------------------
-- Kind and Piece
---------------------------------------------------------------

data Kind
    = KComment
    | KDeclaration
    | KAction
    | KIOBind
    | KTHSplice
    deriving (Show, Eq)

data Piece
    = PBlank
    | PGhciCommand Text
    | PPragma Text
    | PImport Text
    | PUnit Kind [Line]
    deriving (Show)

---------------------------------------------------------------
-- Step 1: [Line] -> [Piece]
---------------------------------------------------------------

toPieces :: [Line] -> [Piece]
toPieces [] = []
toPieces (Blank : rest) = PBlank : toPieces rest
toPieces (GhciCommand t : rest) = PGhciCommand t : toPieces rest
toPieces (Pragma t : rest) = PPragma t : toPieces rest
toPieces (Import t : rest) = PImport t : toPieces rest
toPieces (HaskellLine t : rest)
    | isCommentText t =
        let (more, rest') = spanComments rest
            unit = HaskellLine t : more
         in PUnit KComment unit : toPieces rest'
    | otherwise =
        let (cont, rest') = takeContinuations rest
            unit = HaskellLine t : cont
            k = classify t (map lineText cont)
         in PUnit k unit : toPieces rest'

spanComments :: [Line] -> ([Line], [Line])
spanComments (HaskellLine t : rest)
    | isCommentText t =
        let (more, rest') = spanComments rest
         in (HaskellLine t : more, rest')
spanComments xs = ([], xs)

{- | Take indented continuations after a lead, absorbing interior blank
lines only when followed by more indented non-blank content. Trailing
blanks are left behind so they can serve as unit separators (which is
what makes the "double blank still breaks" case work).
-}
takeContinuations :: [Line] -> ([Line], [Line])
takeContinuations [] = ([], [])
takeContinuations xs@(l : rest)
    | isIndentedNonBlank l =
        let (more, rest') = takeContinuations rest
         in (l : more, rest')
    | isBlankLine l =
        let (blanks, afterBlanks) = span isBlankLine xs
         in case afterBlanks of
                (x : _)
                    | isIndentedNonBlank x ->
                        let (more, rest') = takeContinuations afterBlanks
                         in (blanks ++ more, rest')
                _ -> ([], xs)
    | otherwise = ([], xs)

isIndentedNonBlank :: Line -> Bool
isIndentedNonBlank (HaskellLine t) = T.isPrefixOf " " t || T.isPrefixOf "\t" t
isIndentedNonBlank _ = False

isBlankLine :: Line -> Bool
isBlankLine Blank = True
isBlankLine _ = False

---------------------------------------------------------------
-- Classification
---------------------------------------------------------------

classify :: Text -> [Text] -> Kind
classify leadText contTexts
    | isTHSplice leadText = KTHSplice
    | isDeclaration leadText contTexts = KDeclaration
    | isIOBindLead leadText = KIOBind
    | otherwise = KAction

isTHSplice :: Text -> Bool
isTHSplice t =
    let s = T.stripStart t
     in "$(" `T.isPrefixOf` s || "_ = ();" `T.isPrefixOf` s

isIOBindLead :: Text -> Bool
isIOBindLead = T.isInfixOf "<-"

isDeclaration :: Text -> [Text] -> Bool
isDeclaration leadText contTexts =
    startsWithDeclKeyword leadText
        || isTypeSig leadText
        || isValueBinding leadText
        || isClauseHead leadText && any contHasBinding contTexts

startsWithDeclKeyword :: Text -> Bool
startsWithDeclKeyword t =
    any
        (`T.isPrefixOf` T.stripStart t)
        ["data ", "newtype ", "type ", "class ", "instance ", "default "]

isTypeSig :: Text -> Bool
isTypeSig t = case afterLeadIdent t of
    Just rest -> "::" `T.isPrefixOf` T.stripStart rest
    Nothing -> False

isValueBinding :: Text -> Bool
isValueBinding t = maybe False hasTopLevelEquals (afterLeadIdent t)

{- | A line like @isPrime n@ — identifier + args, no @=@ / @::@ / @<-@ on
this line. Only counts as a declaration when accompanied by indented
continuations that supply the RHS (guards, a @where@ clause, etc.).
-}
isClauseHead :: Text -> Bool
isClauseHead t = case afterLeadIdent t of
    Just rest ->
        let rest' = T.stripEnd (T.stripStart rest)
         in not (T.null rest')
                && not (hasTopLevelEquals rest)
                && not ("::" `T.isInfixOf` rest)
                && not ("<-" `T.isInfixOf` rest)
    Nothing -> False

contHasBinding :: Text -> Bool
contHasBinding t =
    let s = T.stripStart t
     in "| " `T.isPrefixOf` s && hasTopLevelEquals s
            || hasTopLevelEquals t
            || "where" `T.isPrefixOf` s

{- | Parse a leading lowercase identifier. Returns the text that follows
it (with any leading whitespace) or 'Nothing' if the stripped line does
not begin with a lowercase identifier, or begins with a Haskell keyword.
-}
afterLeadIdent :: Text -> Maybe Text
afterLeadIdent t =
    let s = T.stripStart t
     in case T.uncons s of
            Just (c, _)
                | isIdentStart c ->
                    let (ident, rest) = T.span isIdentCont s
                     in if T.null ident || isHaskellKeyword ident
                            then Nothing
                            else Just rest
            _ -> Nothing

isIdentStart :: Char -> Bool
isIdentStart c = c == '_' || isAsciiLower c

isIdentCont :: Char -> Bool
isIdentCont c =
    isIdentStart c
        || isAsciiUpper c
        || isDigit c
        || c == '\''

isHaskellKeyword :: Text -> Bool
isHaskellKeyword t =
    t
        `elem` [ "do"
               , "let"
               , "in"
               , "if"
               , "then"
               , "else"
               , "case"
               , "of"
               , "where"
               , "data"
               , "newtype"
               , "type"
               , "class"
               , "instance"
               , "module"
               , "import"
               , "default"
               , "deriving"
               , "infix"
               , "infixl"
               , "infixr"
               ]

hasTopLevelEquals :: Text -> Bool
hasTopLevelEquals t = " = " `T.isInfixOf` t || T.isSuffixOf " =" t

isCommentText :: Text -> Bool
isCommentText t = "--" `T.isPrefixOf` T.stripStart t

---------------------------------------------------------------
-- Step 2: [Piece] -> [Block]
---------------------------------------------------------------

{- | Normalize a piece stream: attach each comment unit forward onto the
following non-comment unit, and merge runs of adjacent declarations into a
single unit. Shared by 'toGhciScript' (block wrapping) and 'toModule'
(bucketing) so both see identical grouping.
-}
mergePieces :: [Piece] -> [Piece]
mergePieces (PUnit KComment l1 : PUnit k l2 : rest)
    | k /= KComment = mergePieces (PUnit k (l1 ++ l2) : rest)
mergePieces (PUnit KDeclaration l1 : PUnit KDeclaration l2 : rest) =
    mergePieces (PUnit KDeclaration (l1 ++ l2) : rest)
mergePieces (p : rest) = p : mergePieces rest
mergePieces [] = []

piecesToBlocks :: [Piece] -> [Block]
piecesToBlocks = map pieceToBlock . mergePieces
  where
    pieceToBlock PBlank = SingleLine Blank
    pieceToBlock (PGhciCommand t) = SingleLine (GhciCommand t)
    pieceToBlock (PPragma t) = SingleLine (Pragma t)
    pieceToBlock (PImport t) = SingleLine (Import t)
    pieceToBlock (PUnit _ [l]) = SingleLine l
    pieceToBlock (PUnit _ ls) = MultiLine ls

---------------------------------------------------------------
-- Rendering
---------------------------------------------------------------

renderBlock :: Block -> [Text]
renderBlock (SingleLine Blank) = [""]
renderBlock (SingleLine (GhciCommand t)) = [t]
renderBlock (SingleLine (Pragma t)) = [t]
renderBlock (SingleLine (Import t)) = [t]
renderBlock (SingleLine (HaskellLine t)) = wrapMulti [t]
renderBlock (MultiLine ls) = wrapMulti (map lineText ls)

wrapMulti :: [Text] -> [Text]
wrapMulti ls = [":{"] ++ ls ++ [":}"]

lineText :: Line -> Text
lineText Blank = ""
lineText (GhciCommand t) = t
lineText (Pragma t) = t
lineText (Import t) = t
lineText (HaskellLine t) = t

---------------------------------------------------------------
-- Module rendering: notebook cells -> standalone Haskell
---------------------------------------------------------------

{- | The four buckets a sequence of notebook 'Line's sorts into when
assembling a compilable module: language pragmas and imports (hoisted to the
top), top-level declarations (order-independent), and statements destined for
a generated @main@ do-block (which must preserve document order).
-}
data ModuleParts = ModuleParts
    { mpPragmas :: [Text]
    , mpImports :: [Text]
    , mpDecls :: [Text]
    , mpMain :: [Text]
    }
    deriving (Show, Eq)

instance Semigroup ModuleParts where
    a <> b =
        ModuleParts
            { mpPragmas = mpPragmas a <> mpPragmas b
            , mpImports = mpImports a <> mpImports b
            , mpDecls = mpDecls a <> mpDecls b
            , mpMain = mpMain a <> mpMain b
            }

instance Monoid ModuleParts where
    mempty = ModuleParts [] [] [] []

{- | How a trailing bare expression (a 'KAction' unit) should be emitted in
the generated @main@. A GHCi cell that ends in a bare expression auto-prints
it; a compiled @main@ cannot, so the caller resolves each expression's intent
(typically by querying a live session) and 'toModule' emits accordingly.
-}
data TrailKind
    = -- | @IO ()@: emit the expression verbatim as a statement.
      TrailIOUnit
    | -- | @IO a@, @a@ showable and @/= ()@: emit @print =<< (e)@.
      TrailIOShow
    | -- | pure and showable: emit @print (e)@ (the GHCi auto-print case).
      TrailPure
    | -- | undeterminable or not printable: emit commented out.
      TrailUnknown
    deriving (Show, Eq)

-- | Decide how a trailing expression (rendered to 'Text') should be emitted.
type TrailingResolver = Text -> TrailKind

{- | Every trailing-action expression in a line sequence, in order, exactly as
'toModule' presents them to a 'TrailingResolver'. Lets a caller pre-resolve
each expression (e.g. via @:type@ against a live session) and build a pure
resolver before calling 'toModule'.
-}
actionExprs :: [Line] -> [Text]
actionExprs = concatMap fromPiece . mergePieces . toPieces
  where
    fromPiece (PUnit KAction ls) = [actionBody ls]
    fromPiece _ = []

{- | Sort a sequence of notebook 'Line's into 'ModuleParts'. Imports and
language pragmas are hoisted; @:set -XExt@ becomes a @LANGUAGE@ pragma; other
GHCi directives and @:{@\/@:}@ delimiters are dropped; declarations and TH
splices become top-level decls; monadic binds and trailing expressions become
@main@ statements (the latter shaped by the 'TrailingResolver').
-}
toModule :: TrailingResolver -> [Line] -> ModuleParts
toModule resolve = foldMap fromPiece . mergePieces . toPieces
  where
    fromPiece PBlank = mempty
    fromPiece (PGhciCommand t) = ghciToParts t
    fromPiece (PPragma t) = mempty{mpPragmas = [t]}
    fromPiece (PImport t) = mempty{mpImports = [t]}
    fromPiece (PUnit KComment ls) = mempty{mpDecls = [renderLines ls]}
    fromPiece (PUnit KDeclaration ls) = mempty{mpDecls = [renderLines ls]}
    fromPiece (PUnit KTHSplice ls) = mempty{mpDecls = [unRewriteSplice (renderLines ls)]}
    fromPiece (PUnit KIOBind ls) = mempty{mpMain = [renderLines ls]}
    fromPiece (PUnit KAction ls) =
        let (comments, body) = spanCommentLines ls
            expr = renderLines body
            stmt = case resolve expr of
                TrailIOUnit -> expr
                TrailIOShow -> wrapApplied "print =<<" expr
                TrailPure -> wrapApplied "print" expr
                TrailUnknown -> commentOut expr
         in mempty{mpMain = map lineText comments ++ [stmt]}

{- | A GHCi directive's contribution to a module: @:set -XExt@ becomes a
pragma; @:{@\/@:}@ and everything else is dropped.
-}
ghciToParts :: Text -> ModuleParts
ghciToParts t
    | s == ":{" || s == ":}" = mempty
    | otherwise = mempty{mpPragmas = map languagePragma (setExtensions s)}
  where
    s = T.strip t

-- | Extensions named by a @:set -XExt@ \/ @:seti -XExt@ directive.
setExtensions :: Text -> [Text]
setExtensions t = case T.words t of
    (cmd : rest)
        | cmd `elem` [":set", ":seti"] ->
            [ext | w <- rest, Just ext <- [T.stripPrefix "-X" w]]
    _ -> []

languagePragma :: Text -> Text
languagePragma ext = "{-# LANGUAGE " <> ext <> " #-}"

-- | Reverse the parser's GHCi TH hack (@$(x)@ -> @_ = (); x@) for a module.
unRewriteSplice :: Text -> Text
unRewriteSplice t
    | "\n" `T.isInfixOf` t = t
    | Just inner <- T.stripPrefix "_ = (); " (T.stripStart t) = "$(" <> inner <> ")"
    | otherwise = t

-- | @prefix (expr)@, wrapping multi-line expressions onto their own lines.
wrapApplied :: Text -> Text -> Text
wrapApplied prefix expr
    | "\n" `T.isInfixOf` expr =
        prefix <> " (\n" <> indentText "    " expr <> "\n    )"
    | otherwise = prefix <> " (" <> expr <> ")"

commentOut :: Text -> Text
commentOut expr =
    T.intercalate "\n" $
        "-- [sabela:export] could not resolve this trailing expression's type;"
            : "-- left commented out — wire it into main as needed:"
            : map ("-- " <>) (T.lines expr)

actionBody :: [Line] -> Text
actionBody = renderLines . snd . spanCommentLines

spanCommentLines :: [Line] -> ([Line], [Line])
spanCommentLines = span isCommentLine

isCommentLine :: Line -> Bool
isCommentLine (HaskellLine t) = isCommentText t
isCommentLine _ = False

renderLines :: [Line] -> Text
renderLines = T.intercalate "\n" . map lineText

indentText :: Text -> Text -> Text
indentText pad = T.intercalate "\n" . map (pad <>) . T.lines

-- | Deduplicate while preserving first-occurrence order (no @containers@ dep).
dedup :: [Text] -> [Text]
dedup = go []
  where
    go _ [] = []
    go seen (x : xs)
        | x `elem` seen = go seen xs
        | otherwise = x : go (x : seen) xs

{- | Assemble 'ModuleParts' into module source: deduped pragmas, an optional
@module … where@ header, deduped imports, declarations (blank-separated), and
a generated @main@ (@pure ()@ when there are no statements).
-}
renderModuleText :: Maybe Text -> ModuleParts -> Text
renderModuleText mModName mp =
    T.unlines . intercalate [""] . filter (not . null) $
        [ dedup (mpPragmas mp)
        , maybe [] (\n -> ["module " <> n <> " where"]) mModName
        , dedup (mpImports mp)
        , joinBlocks (mpDecls mp)
        , mainLines
        ]
  where
    mainLines =
        ("main :: IO ()" :) $
            case mpMain mp of
                [] -> ["main = pure ()"]
                stmts -> "main = do" : concatMap (map ("    " <>) . T.lines) stmts

-- | Flatten multi-line chunks into lines, separated by a single blank line.
joinBlocks :: [Text] -> [Text]
joinBlocks = intercalate [""] . map T.lines

{- | A single-file cabal-script header (@{\- cabal: … -\}@) rendered from
'CabalMeta'. @base@ is always present and @-Wno-unused-imports@ is added since
the exporter over-includes imports to keep dependency slices self-contained.
-}
renderCabalScriptHeader :: CabalMeta -> Text
renderCabalScriptHeader (CabalMeta deps exts opts) =
    T.unlines $
        ["{- cabal:", "build-depends: " <> commaList (dedup ("base" : deps))]
            ++ ["default-extensions: " <> commaList exts' | not (null exts')]
            ++ ["ghc-options: " <> T.unwords opts']
            ++ ["-}"]
  where
    exts' = dedup exts
    opts' = dedup (opts ++ ["-Wno-unused-imports"])
    commaList = T.intercalate ", "

-- | A runnable single-file cabal script: a @{\- cabal: -\}@ header over a module.
renderCabalScript :: CabalMeta -> ModuleParts -> Text
renderCabalScript meta mp =
    renderCabalScriptHeader meta <> "\n" <> renderModuleText (Just "Main") mp

-- | One block of a literate-Haskell document: prose, or Bird-style code.
data LhsBlock
    = LhsProse Text
    | LhsCode [Text]
    deriving (Show, Eq)

{- | Render literate Haskell, Bird-style (@>@-prefixed code). Blocks are
separated by a blank line, satisfying the rule that a code block must be
preceded and followed by a blank line.
-}
renderLiterate :: [LhsBlock] -> Text
renderLiterate = T.intercalate "\n\n" . map render
  where
    render (LhsProse t) = T.intercalate "\n" (map escapeProse (T.lines t))
    render (LhsCode ls) = T.intercalate "\n" (map bird ls)
    bird l = if T.null l then ">" else "> " <> l
    -- A prose line starting with @>@ would be read as code, and one starting
    -- with @#@ as a CPP line directive; indent both by a space to keep them
    -- prose. (GHC's literate preprocessor is sensitive to column 0.)
    escapeProse l
        | T.isPrefixOf ">" l || T.isPrefixOf "#" l = " " <> l
        | otherwise = l
