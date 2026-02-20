{- | Rendering 'ScriptHs.Parser.Line' sequences into GHCi-compatible scripts.

GHCi imposes constraints that raw Haskell source does not: multi-line
definitions must be wrapped in @:{@ \/ @:}@ blocks, and monadic binds
(@<-@) or Template Haskell splices (@$(@) must be issued as individual
statements rather than grouped with pure definitions. 'toGhciScript'
handles all of this automatically
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

Lines are grouped into blocks and wrapped in @:{@ \/ @:}@ where necessary.
Monadic bind expressions (@<-@) and Template Haskell splices (@$(@) are
always emitted as individual GHCi statements, since GHCi does not allow
them inside multi-line blocks.

Example — a multi-line definition is wrapped in a single block:

@
toGhciScript
  [ HaskellLine "double :: Int -> Int"
  , HaskellLine "double = (*2)"
  ]
-- :{
-- double :: Int -> Int
-- double = (*2)
-- :}
@

Example — an IO bind is kept as a standalone statement:

@
toGhciScript
  [ HaskellLine "x <- getLine"
  , HaskellLine "putStrLn x"
  ]
-- x <- getLine
-- putStrLn x
@
-}
toGhciScript :: [Line] -> Text
toGhciScript = T.unlines . concatMap renderBlock . groupBlocks

groupBlocks :: [Line] -> [Block]
groupBlocks = concatMap splitIOBinds . groupRaw

groupRaw :: [Line] -> [Block]
groupRaw [] = []
groupRaw (Blank : rest) = SingleLine Blank : groupRaw rest
groupRaw (GhciCommand t : rest) = SingleLine (GhciCommand t) : groupRaw rest
groupRaw ls =
    let (block, rest) = span isBlockLine ls
        (block', rest') = takeIfIndented block rest
     in classifyBlock block' : groupRaw rest'

takeIfIndented :: [Line] -> [Line] -> ([Line], [Line])
takeIfIndented block rest = (block ++ takeWhile isIndented rest, dropWhile isIndented rest)

isIndented :: Line -> Bool
isIndented Blank = True
isIndented (HaskellLine t) = T.isPrefixOf " " t || T.isPrefixOf "\t" t
isIndented _ = False

splitIOBinds :: Block -> [Block]
splitIOBinds (MultiLine ls) = map classifyBlock (splitOn isIOLine ls)
  where
    isIOLine l = isIOorTH (lineText l)
splitIOBinds b = [b]

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x : xs)
    | p x = [x] : splitOn p xs
    | otherwise =
        let (run, rest) = break p xs
         in (x : run) : splitOn p rest

isBlockLine :: Line -> Bool
isBlockLine Blank = False
isBlockLine (GhciCommand _) = False
isBlockLine _ = True

classifyBlock :: [Line] -> Block
classifyBlock [l] = SingleLine l
classifyBlock ls = MultiLine ls

renderBlock :: Block -> [Text]
renderBlock (SingleLine Blank) = [""]
renderBlock (SingleLine (GhciCommand t)) = [t]
renderBlock (SingleLine (Pragma t)) = [t]
renderBlock (SingleLine (Import t)) = [t]
renderBlock (SingleLine (HaskellLine t))
    | isIOorTH t = wrapMulti [t]
    | otherwise = [t]
renderBlock (MultiLine ls)
    | allIOorTH ls = concatMap (\l -> wrapMulti [lineText l]) ls
    | otherwise = wrapMulti (map lineText ls)

wrapMulti :: [Text] -> [Text]
wrapMulti ls = [":{"] ++ ls ++ [":}"]

lineText :: Line -> Text
lineText Blank = ""
lineText (GhciCommand t) = t
lineText (Pragma t) = t
lineText (Import t) = t
lineText (HaskellLine t) = t

isIOorTH :: Text -> Bool
isIOorTH t =
    not (T.isPrefixOf " " t)
        && ( T.isInfixOf "<-" t
                || T.isInfixOf "$(" t
                || T.isPrefixOf "_ = ();" (T.stripStart t)
           )

allIOorTH :: [Line] -> Bool
allIOorTH = all (isIOorTH . lineText)
