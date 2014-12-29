module Paths_automaton (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/siddharth/.cabal/bin"
libdir     = "/home/siddharth/.cabal/lib/automaton-0.1.0.0/ghc-7.4.1"
datadir    = "/home/siddharth/.cabal/share/automaton-0.1.0.0"
libexecdir = "/home/siddharth/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "automaton_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "automaton_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "automaton_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "automaton_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
