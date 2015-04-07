module Paths_CowTable5PostgreSql (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/hirofumi/yesod/CowTable1/HDBC/.cabal-sandbox/bin"
libdir     = "/home/hirofumi/yesod/CowTable1/HDBC/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.4/CowTable5PostgreSql-0.1.0.0"
datadir    = "/home/hirofumi/yesod/CowTable1/HDBC/.cabal-sandbox/share/x86_64-linux-ghc-7.8.4/CowTable5PostgreSql-0.1.0.0"
libexecdir = "/home/hirofumi/yesod/CowTable1/HDBC/.cabal-sandbox/libexec"
sysconfdir = "/home/hirofumi/yesod/CowTable1/HDBC/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CowTable5PostgreSql_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CowTable5PostgreSql_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "CowTable5PostgreSql_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CowTable5PostgreSql_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CowTable5PostgreSql_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
