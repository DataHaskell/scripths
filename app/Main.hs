module Main where

import Control.Monad (unless, when)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import qualified Data.Text.IO as TIO
import System.Directory (
    doesDirectoryExist,
    listDirectory,
    makeAbsolute,
 )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, stderr)

import ScriptHs.Notebook (runNotebook)
import ScriptHs.Parser (parseScript)
import ScriptHs.Run (RunOptions (..), defaultRunOptions, runScript)

-- | Parsed command-line options.
data Args = Args
    { argScript :: Maybe FilePath
    , argOutput :: Maybe FilePath
    , argPackages :: [FilePath]
    , argNoLocalProject :: Bool
    , argInPlace :: Bool
    , argHelp :: Bool
    }

emptyArgs :: Args
emptyArgs = Args Nothing Nothing [] False False False

main :: IO ()
main = do
    raw <- getArgs
    case parseArgs raw of
        Left err -> hPutStrLn stderr ("scripths: " ++ err) >> usage
        Right a
            | argHelp a -> help
            | otherwise -> case argScript a of
                Nothing -> usage
                Just path -> do
                    outPath <- resolveOutput a path
                    pkgs <- mapM resolvePackageDir (argPackages a)
                    let opts =
                            defaultRunOptions
                                { roPackages = pkgs
                                , roEnclosingProject = not (argNoLocalProject a)
                                }
                    dispatch opts path outPath

{- | Resolve the output destination, honouring @--in-place@: write back over the
notebook itself. In-place is only meaningful for notebooks (the @.ghci@\/@.hs@
path streams to stdout) and conflicts with an explicit @-o@.
-}
resolveOutput :: Args -> FilePath -> IO (Maybe FilePath)
resolveOutput a path
    | argInPlace a && isJust (argOutput a) =
        die "--in-place cannot be combined with -o/--output"
    | argInPlace a =
        if isNotebook path
            then pure (Just path)
            else die "--in-place only applies to .md/.markdown notebooks"
    | otherwise = pure (argOutput a)

isNotebook :: FilePath -> Bool
isNotebook path = takeExtension path `elem` [".md", ".markdown"]

dispatch :: RunOptions -> FilePath -> Maybe FilePath -> IO ()
dispatch opts path outputPath =
    case takeExtension path of
        ".md" -> runNotebook opts path outputPath
        ".markdown" -> runNotebook opts path outputPath
        _ -> do
            contents <- TIO.readFile path
            let sf = parseScript contents
            runScript opts path sf

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
        | "-" `isPrefixOf` tok = Left ("unknown flag: " ++ tok)
        | otherwise = case argScript a of
            Nothing -> go a{argScript = Just tok} rest
            Just _ -> Left ("unexpected extra argument: " ++ tok)
    stripFlag p s = if p `isPrefixOf` s then Just (drop (length p) s) else Nothing

-- | Resolve a @--package@ dir to absolute, requiring a package root (a @.cabal@).
resolvePackageDir :: FilePath -> IO FilePath
resolvePackageDir dir = do
    path <- makeAbsolute dir
    isDir <- doesDirectoryExist path
    unless isDir (die ("--package: not a directory: " ++ path))
    cabals <- filter ((== ".cabal") . takeExtension) <$> listDirectory path
    when (null cabals) $
        die ("--package: no .cabal file in " ++ path ++ " (expected a package root)")
    pure path

die :: String -> IO a
die msg = hPutStrLn stderr ("scripths: " ++ msg) >> exitFailure

-- | Full help, to stdout, exit success.
help :: IO ()
help = getProgName >>= putStr . helpText >> exitSuccess

-- | Synopsis + a parse-error hint, to stderr, exit failure.
usage :: IO ()
usage = getProgName >>= hPutStrLn stderr . helpText >> exitFailure

helpText :: String -> String
helpText prog =
    unlines
        [ "Usage: " ++ prog ++ " [OPTIONS] <script>"
        , ""
        , "Run a .ghci/.hs script, or a .md/.markdown notebook (rendering each"
        , "code block's output inline)."
        , ""
        , "Options:"
        , "  -o FILE, --output=FILE   write notebook output to FILE"
        , "  -i, --in-place           rewrite the notebook in place (overwrite <script>)"
        , "  -p DIR, --package DIR     add a local package dir (also --package=DIR)"
        , "  --no-local-project        do not auto-include the enclosing cabal project"
        , "  -h, --help                show this help"
        , ""
        , "In-script directives (lines beginning '-- cabal:'):"
        , "  -- cabal: build-depends: pkg1, pkg2"
        , "  -- cabal: default-extensions: OverloadedStrings, LambdaCase"
        , "  -- cabal: ghc-options: -Wall"
        , "  -- cabal: packages: ../sibling-pkg        (extra local package dirs)"
        , "  -- cabal: source-repository-package: <git-url> <ref> [subdir]"
        , ""
        , "Each code block should end in a single bare expression to be auto-printed;"
        , "a block ending in a '<-' bind prints nothing — repeat the bound name on a"
        , "final line to print it."
        ]
