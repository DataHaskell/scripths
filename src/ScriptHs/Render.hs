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
) where

import Data.Text (Text)
import qualified Data.Text as T
import ScriptHs.Parser (Line (..))

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
        || (isClauseHead leadText && any contHasBinding contTexts)

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
isValueBinding t = case afterLeadIdent t of
    Just rest -> hasTopLevelEquals rest
    Nothing -> False

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
     in ("| " `T.isPrefixOf` s && hasTopLevelEquals s)
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
isIdentStart c = c == '_' || (c >= 'a' && c <= 'z')

isIdentCont :: Char -> Bool
isIdentCont c =
    isIdentStart c
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
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

piecesToBlocks :: [Piece] -> [Block]
piecesToBlocks [] = []
piecesToBlocks (PBlank : rest) = SingleLine Blank : piecesToBlocks rest
piecesToBlocks (PGhciCommand t : rest) = SingleLine (GhciCommand t) : piecesToBlocks rest
piecesToBlocks (PPragma t : rest) = SingleLine (Pragma t) : piecesToBlocks rest
piecesToBlocks (PImport t : rest) = SingleLine (Import t) : piecesToBlocks rest
piecesToBlocks (PUnit KComment lines1 : PUnit k lines2 : rest)
    | k /= KComment =
        piecesToBlocks (PUnit k (lines1 ++ lines2) : rest)
piecesToBlocks (PUnit KDeclaration lines1 : PUnit KDeclaration lines2 : rest) =
    piecesToBlocks (PUnit KDeclaration (lines1 ++ lines2) : rest)
piecesToBlocks (PUnit _ ls : rest) = toBlock ls : piecesToBlocks rest
  where
    toBlock [l] = SingleLine l
    toBlock xs = MultiLine xs

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
