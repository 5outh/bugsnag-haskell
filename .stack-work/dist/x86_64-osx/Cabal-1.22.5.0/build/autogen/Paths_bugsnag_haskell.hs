module Paths_bugsnag_haskell (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Ben/projects/bugsnag-haskell/.stack-work/install/x86_64-osx/lts-6.3/7.10.3/bin"
libdir     = "/Users/Ben/projects/bugsnag-haskell/.stack-work/install/x86_64-osx/lts-6.3/7.10.3/lib/x86_64-osx-ghc-7.10.3/bugsnag-haskell-0.1.0.0-CMldjK77m5c9DwfVRIw7lb"
datadir    = "/Users/Ben/projects/bugsnag-haskell/.stack-work/install/x86_64-osx/lts-6.3/7.10.3/share/x86_64-osx-ghc-7.10.3/bugsnag-haskell-0.1.0.0"
libexecdir = "/Users/Ben/projects/bugsnag-haskell/.stack-work/install/x86_64-osx/lts-6.3/7.10.3/libexec"
sysconfdir = "/Users/Ben/projects/bugsnag-haskell/.stack-work/install/x86_64-osx/lts-6.3/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bugsnag_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bugsnag_haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "bugsnag_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bugsnag_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bugsnag_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
