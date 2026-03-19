{-# LANGUAGE NamedFieldPuns #-}

module ScriptHs.Run where

import Control.Monad (unless)
import Data.Char (isAlphaNum)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import ScriptHs.Parser (
    CabalMeta (..),
    Line,
    ScriptFile (scriptLines, scriptMeta),
 )
import ScriptHs.Render (toGhciScript)
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    getCurrentDirectory,
    getHomeDirectory,
    makeAbsolute,
 )
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (stderr)
import System.Process (
    CreateProcess (cwd, delegate_ctlc),
    createProcess,
    proc,
    readCreateProcessWithExitCode,
    waitForProcess,
 )

runScript :: FilePath -> ScriptFile -> IO ()
runScript = runWithCont runGhc

runScriptCapture :: FilePath -> ScriptFile -> IO T.Text
runScriptCapture = runWithCont captureGhc

runWithCont ::
    (FilePath -> FilePath -> FilePath -> IO a) -> FilePath -> ScriptFile -> IO a
runWithCont cont scriptPath sf = do
    scriptAbsPath <- makeAbsolute scriptPath
    userCwd <- getCurrentDirectory
    projectDir <- ensureProject scriptAbsPath (scriptMeta sf) (scriptLines sf)
    cont userCwd projectDir (projectDir </> "script.ghci")

deriveProjectName :: FilePath -> String
deriveProjectName path =
    let sanitized = map (\c -> if isAlphaNum c then c else '-') path
     in dropWhile (== '-') sanitized

ensureProject :: FilePath -> CabalMeta -> [Line] -> IO FilePath
ensureProject scriptAbsPath meta scriptCode = do
    home <- getHomeDirectory
    let name = deriveProjectName scriptAbsPath
        projectDir = home </> ".scripths" </> name
    createDirectoryIfMissing True projectDir
    let cabalProjectPath = projectDir </> "cabal.project"
    cabalProjectExists <- doesFileExist cabalProjectPath
    unless cabalProjectExists $ writeFile cabalProjectPath "packages: .\n"
    let mainHsPath = projectDir </> "Main.hs"
    mainHsExists <- doesFileExist mainHsPath
    unless mainHsExists $ writeFile mainHsPath "main :: IO ()\nmain = pure ()\n"
    writeFile (projectDir </> (name ++ ".cabal")) (renderCabalFile name meta)
    TIO.writeFile (projectDir </> "script.ghci") (toGhciScript scriptCode)
    pure projectDir

renderCabalFile :: String -> CabalMeta -> String
renderCabalFile name CabalMeta{metaDeps, metaExts, metaGhcOptions} =
    let deps = "base" : map T.unpack metaDeps
        depsStr = intercalate ", " deps
        extsStr = intercalate ", " (map T.unpack metaExts)
        optsStr = unwords (map T.unpack metaGhcOptions)
     in unlines
            [ "cabal-version: 3.0"
            , "name:          " ++ name
            , "version:       0.1.0.0"
            , ""
            , "executable main"
            , "  main-is:             Main.hs"
            , "  hs-source-dirs:      ."
            , "  default-language:    Haskell2010"
            , "  build-depends:       " ++ depsStr
            , "  default-extensions:  " ++ extsStr
            , "  ghc-options:         " ++ optsStr
            ]

cabalArgs :: FilePath -> FilePath -> [String]
cabalArgs projectDir ghciPath =
    [ "repl"
    , "exe:main"
    , "--project-dir=" ++ projectDir
    , "--repl-option=-ghci-script=" ++ ghciPath
    , "--repl-option=-e"
    , "--repl-option=return()"
    ]

captureGhc :: FilePath -> FilePath -> FilePath -> IO T.Text
captureGhc userCwd projectDir ghciPath = do
    let args = "-v0" : cabalArgs projectDir ghciPath
        cp = (proc "cabal" args){cwd = Just userCwd}
    (code, out, err) <- readCreateProcessWithExitCode cp ""
    case code of
        ExitSuccess -> pure (T.pack $ out <> err)
        ExitFailure _ -> do
            TIO.hPutStrLn stderr (T.pack err)
            exitWith code

runGhc :: FilePath -> FilePath -> FilePath -> IO ()
runGhc userCwd projectDir ghciPath = do
    let args = "-v0" : cabalArgs projectDir ghciPath
        cp = (proc "cabal" args){cwd = Just userCwd, delegate_ctlc = True}
    (_, _, _, ph) <- createProcess cp
    code <- waitForProcess ph
    case code of
        ExitSuccess -> pure ()
        ExitFailure _ -> exitWith code
