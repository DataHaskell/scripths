{-# LANGUAGE NamedFieldPuns #-}

{- | Assembling and running the throwaway cabal project scripths drives for a
script or notebook. The GHCi code injected into that project's repl lives in
"ScriptHs.Repl".
-}
module ScriptHs.Run (
    RunOptions (..),
    defaultRunOptions,
    runScript,
    runScriptCapture,

    -- * Internals exposed for testing
    cabalArgs,
    deriveProjectName,
    renderCabalFile,
    renderCabalProject,
    usesTemplateHaskell,
) where

import Control.Monad (filterM, when)
import Data.Bits (xor)
import Data.Char (isAlphaNum, isAscii, ord)
import Data.List (foldl', intercalate, nub)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word (Word32)
import Numeric (showHex)
import ScriptHs.Parser (
    CabalMeta (..),
    Line (..),
    ScriptFile (scriptLines, scriptMeta),
    SourceRepoPin (..),
 )
import ScriptHs.Render (toGhciScript)
import ScriptHs.Repl (
    autoPrintDirective,
    cdDirective,
    compileCdSetup,
    compileCdTo,
    replEvalExpr,
    scrubInternalNames,
 )
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
import System.IO (hSetEncoding, stderr, utf8)
import System.Process (
    CreateProcess (cwd, delegate_ctlc, std_err, std_out),
    StdStream (UseHandle),
    createPipe,
    createProcess,
    proc,
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
    projectDir <-
        ensureProject opts userCwd scriptAbsPath (scriptMeta sf) (scriptLines sf)
    cont userCwd projectDir (projectDir </> "script.ghci")

{- | Source for the throwaway executable's @Main.hs@. It only needs to typecheck
(the repl never runs it), and is written prelude-agnostically so it compiles even
when the notebook enables @NoImplicitPrelude@ for the executable.
-}
syntheticMainHs :: T.Text
syntheticMainHs =
    T.unlines
        [ "import qualified System.IO"
        , "import qualified Control.Applicative"
        , "main :: System.IO.IO ()"
        , "main = Control.Applicative.pure ()"
        ]

{- | Write @content@ to @path@ only when it differs from what's already there, so
a stable file (the synthetic @Main.hs@) is not rewritten — and so recompiled — on
every run, while a stale one from an older scripths is healed. Reads strictly
(via "Data.Text.IO") so the handle is closed before the rewrite.
-}
writeFileIfChanged :: FilePath -> T.Text -> IO ()
writeFileIfChanged path content = do
    exists <- doesFileExist path
    existing <- if exists then TIO.readFile path else pure ""
    when (existing /= content) (TIO.writeFile path content)

-- | Warn (once each) about unrecognised @-- cabal:@ directive keys.
warnUnknownKeys :: [T.Text] -> IO ()
warnUnknownKeys keys =
    mapM_
        (\k -> TIO.hPutStrLn stderr ("scripths: warning: unknown '-- cabal:' key: " <> k))
        (nub keys)

{- | Turn an /absolute/ script path into a valid, collision-free cabal package
name (also used as the @~\/.scripths@ subdir). Only ASCII alphanumerics survive —
other characters (incl. non-ASCII letters cabal would reject) become @-@, runs of
@-@ collapse, and leading\/trailing @-@ are stripped. A readable stem alone would
collide (@foo.md@ and @foo-md@ both sanitise to @foo-md@) and could be empty, so
a stable hash of the full path is appended after an @-h@ marker — distinct paths
never share a project dir, and the trailing component always begins with a letter
(a valid cabal name).
-}
deriveProjectName :: FilePath -> String
deriveProjectName path = stem ++ "-h" ++ pathHash path
  where
    sanitised = trimDash (collapseDashes (map asciiAlnumOrDash path))
    stem = if null sanitised then "scripths-script" else sanitised
    asciiAlnumOrDash c = if isAscii c && isAlphaNum c then c else '-'
    collapseDashes ('-' : xs) = '-' : collapseDashes (dropWhile (== '-') xs)
    collapseDashes (x : xs) = x : collapseDashes xs
    collapseDashes [] = []
    trimDash = f . f where f = reverse . dropWhile (== '-')

{- | A short, stable hex hash of a string (FNV-1a over 'Word32'); used to keep
distinct script paths from colliding on the same @~\/.scripths@ project dir
without pulling in a hashing dependency.
-}
pathHash :: String -> String
pathHash s = showHex (foldl' step 2166136261 s) ""
  where
    step :: Word32 -> Char -> Word32
    step h c = (h `xor` fromIntegral (ord c)) * 16777619

ensureProject ::
    RunOptions -> FilePath -> FilePath -> CabalMeta -> [Line] -> IO FilePath
ensureProject opts userCwd scriptAbsPath meta scriptCode = do
    home <- getHomeDirectory
    let name = deriveProjectName scriptAbsPath
        projectDir = home </> ".scripths" </> name
    createDirectoryIfMissing True projectDir
    enclosing <-
        if roEnclosingProject opts
            then enclosingProjectFor scriptAbsPath (metaDeps meta)
            else pure []
    -- @-- cabal: packages:@ dirs are relative to the script file, not the cwd.
    let scriptDir = takeDirectory scriptAbsPath
    metaPkgDirs <-
        mapM (makeAbsolute . (scriptDir </>) . T.unpack) (metaPackages meta)
    let localPkgs = nub (roPackages opts ++ metaPkgDirs ++ enclosing)
    writeManagedCabalProject
        (projectDir </> "cabal.project")
        (renderCabalProject localPkgs (metaSourceRepos meta))
    -- A local package only imports if its name is also a build-depend; collect
    -- the names so renderCabalFile can add them (and warn on dirs without one).
    localNames <- localPackageNames localPkgs
    -- When the notebook uses Template Haskell, its splices read files at compile
    -- time; bracket the body with compile-time chdirs so those resolve against
    -- the user's tree (see 'compileCdTo'). This needs @template-haskell@.
    let th = usesTemplateHaskell meta scriptCode
        extraDeps = localNames ++ ["template-haskell" | th]
    let mainHsPath = projectDir </> "Main.hs"
    writeFileIfChanged mainHsPath syntheticMainHs
    writeFile
        (projectDir </> (name ++ ".cabal"))
        (renderCabalFile name extraDeps meta)
    let body
            | th =
                compileCdSetup
                    <> compileCdTo userCwd
                    <> toGhciScript scriptCode
                    <> compileCdTo projectDir
            | otherwise = toGhciScript scriptCode
    TIO.writeFile
        (projectDir </> "script.ghci")
        (autoPrintDirective <> cdDirective userCwd <> body)
    pure projectDir

{- | Does this notebook use Template Haskell (a @TemplateHaskell@ ext/pragma, a
@template-haskell@ dep, or a @$(…)@ splice line)? If so its splices read files at
compile time, so we bracket the body with compile-time chdirs ('compileCdTo').
-}
usesTemplateHaskell :: CabalMeta -> [Line] -> Bool
usesTemplateHaskell meta ls =
    any (T.isInfixOf "TemplateHaskell") (metaExts meta)
        || any ((== "template-haskell") . depPkgName) (metaDeps meta)
        || any isSpliceLine ls
  where
    isSpliceLine (HaskellLine t) =
        let s = T.stripStart t in "$(" `T.isPrefixOf` s || "_ = ();" `T.isPrefixOf` s
    isSpliceLine (Pragma t) = "TemplateHaskell" `T.isInfixOf` t
    isSpliceLine _ = False

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
        mName <- packageNameInDir dir
        case mName of
            Just nm | nm `elem` names -> pure [dir]
            _ ->
                let parent = takeDirectory dir
                 in if parent == dir then pure [] else go parent

-- | Package name from a build-depends item (drops version bounds and @:sublib@).
depPkgName :: T.Text -> T.Text
depPkgName = T.takeWhile (\c -> isAlphaNum c || c == '-' || c == '_') . T.stripStart

-- | The cabal package name declared in @dir@ (from its first @.cabal@ file).
packageNameInDir :: FilePath -> IO (Maybe T.Text)
packageNameInDir dir = do
    cabals <- findCabalFiles dir
    case cabals of
        (cf : _) -> readCabalPackageName (dir </> cf)
        [] -> pure Nothing

{- | Names of the local packages, for adding to the script's @build-depends@ so
their modules are actually in scope. A dir without a @.cabal@ (it'll also be a
bad @packages:@ entry) is warned about and skipped.
-}
localPackageNames :: [FilePath] -> IO [T.Text]
localPackageNames = fmap concat . mapM nameOrWarn
  where
    nameOrWarn dir = do
        mName <- packageNameInDir dir
        case mName of
            Just nm -> pure [nm]
            Nothing -> do
                TIO.hPutStrLn
                    stderr
                    ("scripths: warning: no .cabal package found in " <> T.pack dir)
                pure []

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

{- | Render the synthetic package's @.cabal@. @extraLocalDeps@ are the names of
local packages (from @--package@, @-- cabal: packages:@, or the enclosing
project) added to @build-depends@ so their modules import.
-}
renderCabalFile :: String -> [T.Text] -> CabalMeta -> String
renderCabalFile name extraLocalDeps CabalMeta{metaDeps, metaExts, metaGhcOptions} =
    let deps = nub ("base" : "directory" : map T.unpack (metaDeps ++ extraLocalDeps))
        depsStr = intercalate ", " deps
        -- OverloadedStrings is on by default in every scripths repl.
        exts = nub ("OverloadedStrings" : map T.unpack metaExts)
        extsStr = intercalate ", " exts
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
    , "--repl-option=" ++ T.unpack replEvalExpr
    ]

{- | Run the repl, capturing stdout and stderr as one /ordered/ stream so a
failing block's diagnostics (GHCi writes them to stderr) stay interleaved with
the block markers and render inline. Read to EOF before 'waitForProcess'.
-}
captureGhc :: FilePath -> FilePath -> FilePath -> IO T.Text
captureGhc userCwd projectDir ghciPath = do
    let args = "-v0" : cabalArgs projectDir ghciPath
    (readEnd, writeEnd) <- createPipe
    -- GHC writes UTF-8 (cell output and the Unicode punctuation in its
    -- diagnostics); decode the pipe as UTF-8 so it is not mangled into mojibake.
    hSetEncoding readEnd utf8
    (_, _, _, ph) <-
        createProcess
            (proc "cabal" args)
                { cwd = Just userCwd
                , std_out = UseHandle writeEnd
                , std_err = UseHandle writeEnd
                }
    out <- TIO.hGetContents readEnd
    code <- waitForProcess ph
    case code of
        ExitSuccess -> pure out
        ExitFailure _ -> do
            TIO.hPutStr stderr (scrubInternalNames out)
            exitWith code

{- | Run the repl for @.ghci@\/@.hs@ scripts: stdout is inherited so it streams
live (and @Ctrl-C@ works via @delegate_ctlc@), while stderr is captured, decoded
as UTF-8, and passed through 'scrubInternalNames' before being forwarded — so a
failing script no longer leaks scripths-internal names in its diagnostics. (We
read stderr to EOF as we go, so the pipe never fills; the trade-off is that
stderr is flushed after the run rather than perfectly interleaved with stdout.)
-}
runGhc :: FilePath -> FilePath -> FilePath -> IO ()
runGhc userCwd projectDir ghciPath = do
    let args = "-v0" : cabalArgs projectDir ghciPath
    (errRead, errWrite) <- createPipe
    hSetEncoding errRead utf8
    let cp =
            (proc "cabal" args)
                { cwd = Just userCwd
                , delegate_ctlc = True
                , std_err = UseHandle errWrite
                }
    (_, _, _, ph) <- createProcess cp
    errOut <- TIO.hGetContents errRead
    code <- waitForProcess ph
    TIO.hPutStr stderr (scrubInternalNames errOut)
    case code of
        ExitSuccess -> pure ()
        ExitFailure _ -> exitWith code
