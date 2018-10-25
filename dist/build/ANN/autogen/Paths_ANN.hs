{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ANN (
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

bindir     = "/home/ethan/.cabal/bin"
libdir     = "/home/ethan/.cabal/lib/x86_64-linux-ghc-8.4.3/ANN-0.1.0.0-L9jWkjU22kxHEODpjPUL9A-ANN"
dynlibdir  = "/home/ethan/.cabal/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/ethan/.cabal/share/x86_64-linux-ghc-8.4.3/ANN-0.1.0.0"
libexecdir = "/home/ethan/.cabal/libexec/x86_64-linux-ghc-8.4.3/ANN-0.1.0.0"
sysconfdir = "/home/ethan/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ANN_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ANN_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ANN_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ANN_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ANN_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ANN_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
