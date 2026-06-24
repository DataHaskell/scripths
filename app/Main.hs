module Main where

import Control.Monad (unless, when)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    makeAbsolute,
 )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, stderr)

import ScriptHs.CLI.Types
import ScriptHs.Markdown
import ScriptHs.Notebook (runNotebook)
import ScriptHs.Parser (parseScriptNumbered)
import ScriptHs.Run (RunOptions (..), defaultRunOptions, runScript)
import ScriptHs.Version (
    newerVersionWarning,
    scripthsVersionText,
    tagStyleFor,
    tagVersion,
 )

main :: IO ()
main = do
    raw <- getArgs
    case parseArgs raw of
        Left err -> hPutStrLn stderr ("scripths: " ++ err) >> usage
        Right a
            | argVersion a -> printVersion
            | argHelp a -> help
            | otherwise -> case argScript a of
                Nothing -> usage
                Just path -> do
                    exists <- doesFileExist path
                    unless exists $ die (path ++ ": No such file or directory")
                    warnIfNewer path
                    outPath <- resolveOutput a path
                    pkgs <- mapM resolvePackageDir (argPackages a)
                    let opts =
                            defaultRunOptions
                                { roPackages = pkgs
                                , roEnclosingProject = not (argNoLocalProject a)
                                }
                    let renderOpts =
                            RenderOptions
                                { renderCodeStyle = argCodeStyle a
                                , renderOutputStyle = argOutputStyle a
                                }
                    dispatch renderOpts opts path outPath

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

dispatch ::
    RenderOptions -> RunOptions -> FilePath -> Maybe FilePath -> IO ()
dispatch renderOpts opts path outputPath =
    case takeExtension path of
        ".md" -> runNotebook renderOpts opts path outputPath
        ".markdown" -> runNotebook renderOpts opts path outputPath
        _ -> do
            contents <- TIO.readFile path
            let (sf, numbered) = parseScriptNumbered contents
            runScript opts path sf numbered

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

-- | Print @\<prog\> \<version\>@ to stdout, exit success.
printVersion :: IO ()
printVersion = do
    prog <- getProgName
    putStrLn (prog ++ " " ++ T.unpack scripthsVersionText)
    exitSuccess

{- | Warn (to stderr, then continue) if the file's first-line version tag
declares a scripths newer than this binary — its syntax may not fully parse.
-}
warnIfNewer :: FilePath -> IO ()
warnIfNewer path = do
    contents <- TIO.readFile path
    case tagVersion (tagStyleFor path) contents >>= newerVersionWarning of
        Just w -> hPutStrLn stderr ("scripths: warning: " ++ T.unpack w)
        Nothing -> pure ()

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
        , "  -v, --version             show the scripths version"
        , "  --code-style=display|remove    control how code fences should appear after processing (default: display)"
        , "  --output-style=quoted|raw      control how evaluted code is outputed (default: quoted)"
        , ""
        , "Files may carry a first-line version tag recording the scripths that wrote"
        , "them ('-- scripths: X' in scripts, '<!-- scripths: X -->' in notebooks); a"
        , "file declaring a newer scripths than this binary is run with a warning."
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
