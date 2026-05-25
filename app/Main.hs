module Main where

import Control.Monad (unless, when)
import Data.List (isPrefixOf)
import qualified Data.Text.IO as TIO
import System.Directory (
    doesDirectoryExist,
    listDirectory,
    makeAbsolute,
 )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
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
    }

emptyArgs :: Args
emptyArgs = Args Nothing Nothing [] False

main :: IO ()
main = do
    raw <- getArgs
    case parseArgs raw of
        Left err -> hPutStrLn stderr ("scripths: " ++ err) >> usage
        Right a -> case argScript a of
            Nothing -> usage
            Just path -> do
                pkgs <- mapM resolvePackageDir (argPackages a)
                let opts =
                        defaultRunOptions
                            { roPackages = pkgs
                            , roEnclosingProject = not (argNoLocalProject a)
                            }
                dispatch opts path (argOutput a)

dispatch :: RunOptions -> FilePath -> Maybe FilePath -> IO ()
dispatch opts path outputPath =
    case takeExtension path of
        ".md" -> runNotebook opts path outputPath
        ".markdown" -> runNotebook opts path outputPath
        _ -> do
            contents <- TIO.readFile path
            let sf = parseScript contents
            runScript opts path sf

{- | Parse args: @[-o FILE | --output=FILE] [--package DIR | -p DIR |
--package=DIR]... [--no-local-project] <script>@. The script is the sole
non-flag argument.
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

usage :: IO ()
usage = do
    prog <- getProgName
    putStrLn $
        "Usage: "
            ++ prog
            ++ " [-o FILE | --output=FILE] [-p DIR | --package DIR]..."
            ++ " [--no-local-project] <script>"
    exitFailure
