{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_servant_playground (
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

bindir     = "/home/bolt/Desktop/Bolt/Playground/Haskell/servant-playground/.stack-work/install/x86_64-linux-tinfo6/a959b21616bcae181421d90737df295ace33e78aa0b0896fcaa1b719e866732c/8.6.5/bin"
libdir     = "/home/bolt/Desktop/Bolt/Playground/Haskell/servant-playground/.stack-work/install/x86_64-linux-tinfo6/a959b21616bcae181421d90737df295ace33e78aa0b0896fcaa1b719e866732c/8.6.5/lib/x86_64-linux-ghc-8.6.5/servant-playground-0.1.0.0-9URRKoQ4Yty5SY4jtBl1nx-servant-playground-exe"
dynlibdir  = "/home/bolt/Desktop/Bolt/Playground/Haskell/servant-playground/.stack-work/install/x86_64-linux-tinfo6/a959b21616bcae181421d90737df295ace33e78aa0b0896fcaa1b719e866732c/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/bolt/Desktop/Bolt/Playground/Haskell/servant-playground/.stack-work/install/x86_64-linux-tinfo6/a959b21616bcae181421d90737df295ace33e78aa0b0896fcaa1b719e866732c/8.6.5/share/x86_64-linux-ghc-8.6.5/servant-playground-0.1.0.0"
libexecdir = "/home/bolt/Desktop/Bolt/Playground/Haskell/servant-playground/.stack-work/install/x86_64-linux-tinfo6/a959b21616bcae181421d90737df295ace33e78aa0b0896fcaa1b719e866732c/8.6.5/libexec/x86_64-linux-ghc-8.6.5/servant-playground-0.1.0.0"
sysconfdir = "/home/bolt/Desktop/Bolt/Playground/Haskell/servant-playground/.stack-work/install/x86_64-linux-tinfo6/a959b21616bcae181421d90737df295ace33e78aa0b0896fcaa1b719e866732c/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "servant_playground_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "servant_playground_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "servant_playground_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "servant_playground_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "servant_playground_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "servant_playground_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
