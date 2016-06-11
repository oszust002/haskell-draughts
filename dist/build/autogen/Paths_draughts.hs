module Paths_draughts (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/kamil/.cabal/bin"
libdir     = "/home/kamil/.cabal/lib/draughts-1.0/ghc-7.6.3"
datadir    = "/home/kamil/.cabal/share/draughts-1.0"
libexecdir = "/home/kamil/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "draughts_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "draughts_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "draughts_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "draughts_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
