{-# LANGUAGE NamedFieldPuns #-}

module ScriptHs.Run where

import Control.Monad (filterM, unless, when)
import Data.Char (isAlphaNum)
import Data.List (intercalate, nub)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import ScriptHs.Parser (
    CabalMeta (..),
    Line,
    ScriptFile (scriptLines, scriptMeta),
    SourceRepoPin (..),
 )
import ScriptHs.Render (toGhciScript)
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    getCurrentDirectory,
    getHomeDirectory,
    listDirectory,
    makeAbsolute,
 )
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (stderr)
import System.Process (
    CreateProcess (cwd, delegate_ctlc),
    createProcess,
    proc,
    readCreateProcessWithExitCode,
    waitForProcess,
 )

-- | Options controlling how the throwaway cabal project is assembled.
data RunOptions = RunOptions
    { roPackages :: [FilePath]
    -- ^ Local package directories to add to @packages:@ (already absolute).
    , roEnclosingProject :: Bool
    {- ^ Auto-include the enclosing cabal project when a @build-depends@ names
    it (the zero-config default). Disabled by @--no-local-project@.
    -}
    }
    deriving (Show, Eq)

-- | No explicit @--package@ dirs; enclosing-project auto-detection on.
defaultRunOptions :: RunOptions
defaultRunOptions = RunOptions{roPackages = [], roEnclosingProject = True}

runScript :: RunOptions -> FilePath -> ScriptFile -> IO ()
runScript = runWithCont runGhc

runScriptCapture :: RunOptions -> FilePath -> ScriptFile -> IO T.Text
runScriptCapture = runWithCont captureGhc

runWithCont ::
    (FilePath -> FilePath -> FilePath -> IO a) ->
    RunOptions ->
    FilePath ->
    ScriptFile ->
    IO a
runWithCont cont opts scriptPath sf = do
    scriptAbsPath <- makeAbsolute scriptPath
    userCwd <- getCurrentDirectory
    warnUnknownKeys (metaUnknownKeys (scriptMeta sf))
    projectDir <- ensureProject opts scriptAbsPath (scriptMeta sf) (scriptLines sf)
    cont userCwd projectDir (projectDir </> "script.ghci")

-- | Warn (once each) about unrecognised @-- cabal:@ directive keys.
warnUnknownKeys :: [T.Text] -> IO ()
warnUnknownKeys keys =
    mapM_
        (\k -> TIO.hPutStrLn stderr ("scripths: warning: unknown '-- cabal:' key: " <> k))
        (nub keys)

deriveProjectName :: FilePath -> String
deriveProjectName path =
    let sanitized = map (\c -> if isAlphaNum c then c else '-') path
     in dropWhile (== '-') sanitized

ensureProject :: RunOptions -> FilePath -> CabalMeta -> [Line] -> IO FilePath
ensureProject opts scriptAbsPath meta scriptCode = do
    home <- getHomeDirectory
    let name = deriveProjectName scriptAbsPath
        projectDir = home </> ".scripths" </> name
    createDirectoryIfMissing True projectDir
    enclosing <-
        if roEnclosingProject opts
            then enclosingProjectFor scriptAbsPath (metaDeps meta)
            else pure []
    let localPkgs = nub (roPackages opts ++ enclosing)
    writeManagedCabalProject
        (projectDir </> "cabal.project")
        (renderCabalProject localPkgs (metaSourceRepos meta))
    let mainHsPath = projectDir </> "Main.hs"
    mainHsExists <- doesFileExist mainHsPath
    unless mainHsExists $ writeFile mainHsPath "main :: IO ()\nmain = pure ()\n"
    writeFile (projectDir </> (name ++ ".cabal")) (renderCabalFile name meta)
    TIO.writeFile (projectDir </> "script.ghci") (toGhciScript scriptCode)
    pure projectDir

{- | First line of a scripths-generated @cabal.project@. We regenerate only
files that carry it (or the legacy @packages: .@ default), never hand-edited ones.
-}
managedSentinel :: T.Text
managedSentinel =
    "-- managed by scripths (regenerated each run; remove this line to hand-edit)"

{- | Render a @cabal.project@ from local package dirs + git source-repo pins.
Always includes @.@ (the synthetic script package).
-}
renderCabalProject :: [FilePath] -> [SourceRepoPin] -> T.Text
renderCabalProject localPkgs repos =
    T.unlines $
        [managedSentinel, "packages: ."]
            ++ map (\p -> "          " <> T.pack p) localPkgs
            ++ concatMap renderRepo repos
  where
    renderRepo r =
        [ "source-repository-package"
        , "    type: git"
        , "    location: " <> srpLocation r
        , "    tag: " <> srpRef r
        ]
            ++ maybe [] (\s -> ["    subdir: " <> s]) (srpSubdir r)

{- | Write @cabal.project@ only if absent, scripths-managed (carries the
sentinel), or the legacy @packages: .@ default — never clobber a hand-edited one.
-}
writeManagedCabalProject :: FilePath -> T.Text -> IO ()
writeManagedCabalProject path content = do
    exists <- doesFileExist path
    managed <-
        if not exists
            then pure True
            else do
                existing <- TIO.readFile path
                pure (managedSentinel `T.isPrefixOf` existing || existing == "packages: .\n")
    when managed $ TIO.writeFile path content

{- | If, enclosing the script, there is a cabal project whose package name is
named by a @build-depends@, return that project root (so docs build against the
local working tree). Empty otherwise.
-}
enclosingProjectFor :: FilePath -> [T.Text] -> IO [FilePath]
enclosingProjectFor scriptAbsPath deps = go (takeDirectory scriptAbsPath)
  where
    names = map depPkgName deps
    go dir = do
        cabals <- findCabalFiles dir
        mName <- case cabals of
            (cf : _) -> readCabalPackageName (dir </> cf)
            [] -> pure Nothing
        case mName of
            Just nm | nm `elem` names -> pure [dir]
            _ ->
                let parent = takeDirectory dir
                 in if parent == dir then pure [] else go parent

-- | Package name from a build-depends item (drops version bounds and @:sublib@).
depPkgName :: T.Text -> T.Text
depPkgName = T.takeWhile (\c -> isAlphaNum c || c == '-' || c == '_') . T.stripStart

findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles dir = do
    entries <- listDirectory dir
    -- Keep only regular files: a *directory* named @.cabal@ (cabal's home dir,
    -- which an upward walk reaches) also matches @takeExtension == ".cabal"@.
    filterM
        (doesFileExist . (dir </>))
        [e | e <- entries, takeExtension e == ".cabal"]

readCabalPackageName :: FilePath -> IO (Maybe T.Text)
readCabalPackageName path = do
    contents <- TIO.readFile path
    pure $
        listToMaybe
            [ T.strip (T.drop 1 (T.dropWhile (/= ':') ln))
            | ln <- T.lines contents
            , "name:" `T.isPrefixOf` T.toLower (T.stripStart ln)
            ]

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
