{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_fl2020_spbu_spr (
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

bindir     = "/Users/egorgorbacev/Documents/SPBU_SP_4_term/\1058\1077\1086\1088\1080\1103 \1092\1086\1088\1084\1072\1083\1100\1085\1099\1093 \1103\1079\1099\1082\1086\1074/fl_2020_spbu_spr/.stack-work/install/x86_64-osx/b1b03de92cacc06796ca457a816b2f3870750ed1bd0e86a97b9eeb11364811a2/8.8.2/bin"
libdir     = "/Users/egorgorbacev/Documents/SPBU_SP_4_term/\1058\1077\1086\1088\1080\1103 \1092\1086\1088\1084\1072\1083\1100\1085\1099\1093 \1103\1079\1099\1082\1086\1074/fl_2020_spbu_spr/.stack-work/install/x86_64-osx/b1b03de92cacc06796ca457a816b2f3870750ed1bd0e86a97b9eeb11364811a2/8.8.2/lib/x86_64-osx-ghc-8.8.2/fl2020-spbu-spr-0.1.0.0-B3QAJhXzcYiCwQfi6BQJQJ-polish"
dynlibdir  = "/Users/egorgorbacev/Documents/SPBU_SP_4_term/\1058\1077\1086\1088\1080\1103 \1092\1086\1088\1084\1072\1083\1100\1085\1099\1093 \1103\1079\1099\1082\1086\1074/fl_2020_spbu_spr/.stack-work/install/x86_64-osx/b1b03de92cacc06796ca457a816b2f3870750ed1bd0e86a97b9eeb11364811a2/8.8.2/lib/x86_64-osx-ghc-8.8.2"
datadir    = "/Users/egorgorbacev/Documents/SPBU_SP_4_term/\1058\1077\1086\1088\1080\1103 \1092\1086\1088\1084\1072\1083\1100\1085\1099\1093 \1103\1079\1099\1082\1086\1074/fl_2020_spbu_spr/.stack-work/install/x86_64-osx/b1b03de92cacc06796ca457a816b2f3870750ed1bd0e86a97b9eeb11364811a2/8.8.2/share/x86_64-osx-ghc-8.8.2/fl2020-spbu-spr-0.1.0.0"
libexecdir = "/Users/egorgorbacev/Documents/SPBU_SP_4_term/\1058\1077\1086\1088\1080\1103 \1092\1086\1088\1084\1072\1083\1100\1085\1099\1093 \1103\1079\1099\1082\1086\1074/fl_2020_spbu_spr/.stack-work/install/x86_64-osx/b1b03de92cacc06796ca457a816b2f3870750ed1bd0e86a97b9eeb11364811a2/8.8.2/libexec/x86_64-osx-ghc-8.8.2/fl2020-spbu-spr-0.1.0.0"
sysconfdir = "/Users/egorgorbacev/Documents/SPBU_SP_4_term/\1058\1077\1086\1088\1080\1103 \1092\1086\1088\1084\1072\1083\1100\1085\1099\1093 \1103\1079\1099\1082\1086\1074/fl_2020_spbu_spr/.stack-work/install/x86_64-osx/b1b03de92cacc06796ca457a816b2f3870750ed1bd0e86a97b9eeb11364811a2/8.8.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fl2020_spbu_spr_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fl2020_spbu_spr_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "fl2020_spbu_spr_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "fl2020_spbu_spr_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fl2020_spbu_spr_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fl2020_spbu_spr_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
