module ScriptHs.CLI.Types (
    Args (..),
    emptyArgs,
    parseArgs,
) where

import Data.List (isPrefixOf)
import ScriptHs.Markdown

-- | Parsed command-line options.
data Args = Args
    { argScript :: Maybe FilePath
    , argOutput :: Maybe FilePath
    , argPackages :: [FilePath]
    , argNoLocalProject :: Bool
    , argInPlace :: Bool
    , argHelp :: Bool
    , argVersion :: Bool
    , argCodeStyle :: CodeStyle
    , argOutputStyle :: OutputStyle
    }

emptyArgs :: Args
emptyArgs =
    Args
        { argScript = Nothing
        , argOutput = Nothing
        , argPackages = []
        , argNoLocalProject = False
        , argInPlace = False
        , argHelp = False
        , argVersion = False
        , argCodeStyle = DisplayCode
        , argOutputStyle = OutputQuoted
        }

{- | Parse args: @[-o FILE | --output=FILE] [-i | --in-place] [--package DIR |
-p DIR | --package=DIR]... [--no-local-project] [-h | --help] <script>@. The
script is the sole non-flag argument.
-}
parseArgs :: [String] -> Either String Args
parseArgs = go emptyArgs
  where
    go a [] = Right a
    go a ("-o" : f : rest) = go a{argOutput = Just f} rest
    go _ ["-o"] = Left "-o requires a filename"
    go a (tok : rest)
        | tok == "-p" || tok == "--package" = case rest of
            (d : rest') -> go a{argPackages = argPackages a ++ [d]} rest'
            [] -> Left (tok ++ " requires a directory")
        | Just f <- stripFlag "--output=" tok = go a{argOutput = Just f} rest
        | Just d <- stripFlag "--package=" tok =
            go a{argPackages = argPackages a ++ [d]} rest
        | tok == "--no-local-project" = go a{argNoLocalProject = True} rest
        | tok == "-i" || tok == "--in-place" = go a{argInPlace = True} rest
        | tok == "-h" || tok == "--help" = go a{argHelp = True} rest
        | tok == "-v" || tok == "--version" = go a{argVersion = True} rest
        | tok == "--code-style" = case rest of
            (d : rest') -> case parseCodeStyle d of
                Just codeStyle -> go a{argCodeStyle = codeStyle} rest'
                Nothing -> Left ("Could not parse code style: " ++ tok)
            [] -> Left (tok ++ " requires a code style")
        | tok == "--output-style" = case rest of
            (d : rest') -> case parseOutputStyle d of
                Just codeStyle -> go a{argOutputStyle = codeStyle} rest'
                Nothing -> Left ("Could not parse output style: " ++ tok)
            [] -> Left (tok ++ " requires a output style")
        | "-" `isPrefixOf` tok = Left ("unknown flag: " ++ tok)
        | otherwise = case argScript a of
            Nothing -> go a{argScript = Just tok} rest
            Just _ -> Left ("unexpected extra argument: " ++ tok)
    stripFlag p s = if p `isPrefixOf` s then Just (drop (length p) s) else Nothing
