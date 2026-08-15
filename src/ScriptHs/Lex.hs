{-# LANGUAGE OverloadedStrings #-}

{- | Length-preserving lexical masking for single source lines. The
classifiers in "ScriptHs.Render" decide what a line is from tokens at the
statement level; masking literal contents, bracketed text, and trailing
comments first lets each check stay a plain "Data.Text" search.
-}
module ScriptHs.Lex (
    maskLiterals,
    maskNested,
    stripLineComment,
    isSymbolChar,
) where

import Data.Text (Text)
import qualified Data.Text as T

{- | Blank out string and character literals (delimiters included) with
spaces. A single quote counts as a literal only when one closes it right
after the (possibly escaped) character; otherwise it is a prime in an
identifier like @xs'@.
-}
maskLiterals :: Text -> Text
maskLiterals = T.pack . go . T.unpack
  where
    go [] = []
    go ('"' : cs) = ' ' : inString cs
    go ('\'' : '\\' : c : cs)
        | (esc, '\'' : rest) <- break (== '\'') cs =
            blank (3 + length esc + 1) ++ go rest
        | otherwise = '\'' : go ('\\' : c : cs)
    go ('\'' : c : '\'' : cs)
        | c /= '\\' = blank 3 ++ go cs
    go (c : cs) = c : go cs
    inString [] = []
    inString ('\\' : _ : cs) = ' ' : ' ' : inString cs
    inString ('"' : cs) = ' ' : go cs
    inString (_ : cs) = ' ' : inString cs
    blank n = replicate n ' '

{- | 'maskLiterals', then blank out everything inside (and including)
brackets, leaving only statement-level source visible.
-}
maskNested :: Text -> Text
maskNested = T.pack . go (0 :: Int) . T.unpack . maskLiterals
  where
    go _ [] = []
    go d (c : cs)
        | c `elem` ("([{" :: String) = ' ' : go (d + 1) cs
        | c `elem` (")]}" :: String) = ' ' : go (max 0 (d - 1)) cs
        | d > 0 = ' ' : go d cs
        | otherwise = c : go d cs

{- | Drop a trailing @--@ line comment (and anything after it). Honouring
maximal munch, a dash run extended by other symbol characters (@-->@, @--|@)
is an operator, not a comment; a @--@ inside a literal is text.
-}
stripLineComment :: Text -> Text
stripLineComment t = maybe t (\i -> T.take i t) (commentIndex 0 (maskLiterals t))
  where
    commentIndex i s = case T.uncons s of
        Nothing -> Nothing
        Just (c, rest)
            | isSymbolChar c ->
                let run = T.takeWhile isSymbolChar s
                    n = T.length run
                 in if n >= 2 && T.all (== '-') run
                        then Just i
                        else commentIndex (i + n) (T.drop n s)
            | otherwise -> commentIndex (i + 1) rest

-- | Characters that can extend a Haskell operator lexeme (maximal munch).
isSymbolChar :: Char -> Bool
isSymbolChar c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)
