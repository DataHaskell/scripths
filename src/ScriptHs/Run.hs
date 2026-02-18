{-# LANGUAGE RecordWildCards #-}

module ScriptHs.Run where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process

import ScriptHs.Parser
import ScriptHs.Render

runScript :: ScriptFile -> IO ()
runScript sf = withSystemTempDirectory "scripths" $ \tmpDir -> do
    let ghciPath = tmpDir </> "script.ghci"
        envPath = tmpDir </> ".ghc.environment"
        sm = scriptMeta sf
    TIO.writeFile ghciPath (toGhciScript (scriptLines sf))
    mEnv <- resolveIfNeeded envPath sm
    runGhc mEnv sm ghciPath

resolveIfNeeded :: FilePath -> CabalMeta -> IO (Maybe FilePath)
resolveIfNeeded envPath CabalMeta{..}
    | null metaDeps = pure Nothing
    | otherwise = resolveDeps envPath metaDeps >> pure (Just envPath)

resolveDeps :: FilePath -> [T.Text] -> IO ()
resolveDeps envPath deps = do
    let args =
            ["-v0", "install", "--lib", "--package-env=" ++ envPath]
                ++ map T.unpack deps
        cp = (proc "cabal" args){delegate_ctlc = True}
    (_, _, _, ph) <- createProcess cp
    code <- waitForProcess ph
    case code of
        ExitSuccess -> pure ()
        ExitFailure n -> do
            putStrLn $ "scripths: cabal install --lib failed (exit " ++ show n ++ ")"
            exitWith code

runGhc :: Maybe FilePath -> CabalMeta -> FilePath -> IO ()
runGhc mEnv meta ghciPath = do
    let args = ghcArgs mEnv meta ghciPath
        cp = (proc "ghc" args){delegate_ctlc = True}
    (_, _, _, ph) <- createProcess cp
    code <- waitForProcess ph
    case code of
        ExitSuccess -> pure ()
        ExitFailure _ -> exitWith code

ghcArgs :: Maybe FilePath -> CabalMeta -> FilePath -> [String]
ghcArgs mEnv CabalMeta{..} ghciPath =
    let envFlags = maybe [] (\env -> ["-package-env=" ++ env]) mEnv
        extFlags = map (\e -> "-X" ++ T.unpack e) metaExts
        optFlags = map T.unpack metaGhcOptions
        scriptArg = ":script " ++ ghciPath
     in envFlags ++ extFlags ++ optFlags ++ ["-e", scriptArg]
