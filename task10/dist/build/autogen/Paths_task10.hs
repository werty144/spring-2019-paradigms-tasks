{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_task10 (
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

bindir     = "C:\\Users\\\1069\1076\1075\1072\1088\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\\1069\1076\1075\1072\1088\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3\\task10-0.1.0.0-Icx0d7YZ1DEGDQkt10G1Xs"
dynlibdir  = "C:\\Users\\\1069\1076\1075\1072\1088\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3"
datadir    = "C:\\Users\\\1069\1076\1075\1072\1088\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3\\task10-0.1.0.0"
libexecdir = "C:\\Users\\\1069\1076\1075\1072\1088\\AppData\\Roaming\\cabal\\task10-0.1.0.0-Icx0d7YZ1DEGDQkt10G1Xs\\x86_64-windows-ghc-8.4.3\\task10-0.1.0.0"
sysconfdir = "C:\\Users\\\1069\1076\1075\1072\1088\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "task10_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "task10_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "task10_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "task10_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "task10_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "task10_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
