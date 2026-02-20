{-# LANGUAGE NamedFieldPuns #-}

module ScriptHs.Run where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import ScriptHs.Parser (
    CabalMeta (..),
    ScriptFile (scriptLines, scriptMeta),
 )
import ScriptHs.Render (toGhciScript)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (
    CreateProcess (delegate_ctlc),
    createProcess,
    proc,
    readProcessWithExitCode,
    waitForProcess,
 )

runScript :: ScriptFile -> IO ()
runScript = runWithCont runGhc

runScriptCapture :: ScriptFile -> IO T.Text
runScriptCapture = runWithCont captureGhc

runWithCont :: (FilePath -> CabalMeta -> FilePath -> IO a) -> ScriptFile -> IO a
runWithCont cont sf = withSystemTempDirectory "scripths" $ \tmpDir -> do
    (ghciPath, envPath) <- createScriptAndEnvironment sf tmpDir
    cont envPath (scriptMeta sf) ghciPath

createScriptAndEnvironment :: ScriptFile -> FilePath -> IO (FilePath, FilePath)
createScriptAndEnvironment sf tmpDir = do
    let ghciPath = tmpDir </> "script.ghci"
        envPath = tmpDir </> ".ghc.environment"
        sm = scriptMeta sf
    TIO.writeFile ghciPath (toGhciScript (scriptLines sf))
    resolveDeps envPath (metaDeps sm <> ["base"])
    pure (ghciPath, envPath)

captureGhc :: FilePath -> CabalMeta -> FilePath -> IO T.Text
captureGhc env meta ghciPath = do
    let args = ghcArgs env meta ghciPath
    (code, out, err) <- readProcessWithExitCode "ghc" args ""
    case code of
        ExitSuccess -> pure (T.pack $ out <> err)
        ExitFailure _ -> do
            TIO.hPutStrLn stderr (T.pack err)
            exitWith code

runGhc :: FilePath -> CabalMeta -> FilePath -> IO ()
runGhc env meta ghciPath = do
    let args = ghcArgs env meta ghciPath
        cp = (proc "ghc" args){delegate_ctlc = True}
    (_, _, _, ph) <- createProcess cp
    code <- waitForProcess ph
    case code of
        ExitSuccess -> pure ()
        ExitFailure _ -> exitWith code

ghcArgs :: FilePath -> CabalMeta -> FilePath -> [String]
ghcArgs env CabalMeta{metaExts, metaGhcOptions} ghciPath =
    let envFlags = ["-package-env=" ++ env]
        extFlags = map (\e -> "-X" ++ T.unpack e) metaExts
        optFlags = map T.unpack metaGhcOptions
        scriptArg = ":script " ++ ghciPath
     in envFlags ++ extFlags ++ optFlags ++ ["-e", scriptArg]

resolveDeps :: FilePath -> [T.Text] -> IO ()
resolveDeps _ [] = pure ()
resolveDeps envPath deps = do
    let args =
            ["-v0", "install", "--lib", "--package-env=" ++ envPath, "--force-reinstalls"]
                ++ map T.unpack deps
        cp = (proc "cabal" args){delegate_ctlc = True}
    (_, _, _, ph) <- createProcess cp
    code <- waitForProcess ph
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> do
            TIO.hPutStrLn
                stderr
                (T.pack ("scripths: cabal install --lib failed (exit " ++ show n ++ ")"))
            exitWith code
