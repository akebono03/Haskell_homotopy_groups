{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_spock20240105 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/akebono03/.cabal/bin"
libdir     = "/home/akebono03/.cabal/lib/x86_64-linux-ghc-8.8.4/spock20240105-0.1.0.0-inplace"
dynlibdir  = "/home/akebono03/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/akebono03/.cabal/share/x86_64-linux-ghc-8.8.4/spock20240105-0.1.0.0"
libexecdir = "/home/akebono03/.cabal/libexec/x86_64-linux-ghc-8.8.4/spock20240105-0.1.0.0"
sysconfdir = "/home/akebono03/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "spock20240105_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "spock20240105_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "spock20240105_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "spock20240105_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "spock20240105_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "spock20240105_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
