module HPack.Source.SourceRepo
(SourceRepo, initSourceRepo, destroySourceRepo, getSourceForPkg)
where

import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.IO.Error (IOError, tryIOError)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Control.Monad (forM_)

-- import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

import HPack.Source.Package (Pkg(..), Version, showVersion)
import HPack.Cache (Cache, newCache, addElem)
import HPack.Config (Config(..))

-- | An on-disk repository of Haskell source code, as downloaded from
-- hackage.
data SourceRepo
    = SourceRepo
        { path       :: FilePath
        , cache      :: Cache Pkg
        , config     :: Config
        }

-- | Initialize repository of source code
initSourceRepo :: FilePath -> Config -> IO SourceRepo
initSourceRepo path config = do
    createDirectoryIfMissing False path
    return (SourceRepo path (newCache 1000) config)

-- | Wipe out the source repository
destroySourceRepo :: SourceRepo -> IO ()
destroySourceRepo repo
    = removeDirectoryRecursive (path repo)

-- | Get the source code location for a given package, which is a directory
-- as returned by 'cabal get mypkg-version'.
--
-- If not present in the source repository, run cabal get to get the source
-- code.
getSourceForPkg
    :: SourceRepo
    -> Pkg
    -> IO (SourceRepo, FilePath)
getSourceForPkg repo@(SourceRepo path cache config) pkg@(Pkg name version)
    = do
        -- Fetch sources
        sourcePath <- cabalGetSource repo pkg

        -- Add to cache
        let (cache', deletes) = addElem pkg cache
        let repo' = SourceRepo path cache' config

        -- Delete old entries from cache
        forM_ deletes (deletePkg repo)
        return (repo', sourcePath)

-- | Remove package sources from the file system for the given package
deletePkg :: SourceRepo -> Pkg -> IO ()
deletePkg repo pkg = removeDirectoryRecursive (getSourcePath repo pkg)

-- | Get the location for source code for a package on the file system
getSourcePath :: SourceRepo -> Pkg -> FilePath
getSourcePath repo (Pkg name version)
    = path repo </> name </> showVersion version

-- | Run 'cabal get mypkg-version' to download and untar source
-- code from hackage
cabalGetSource :: SourceRepo -> Pkg -> IO String
cabalGetSource repo pkg@(Pkg name version) = do
    let sourcePath = getSourcePath repo pkg
    let cabal = cabalPath (config repo)
    let args = ["get", name ++ "-" ++ showVersion version]

    -- Invoke 'cabal get mypkg-version'
    (exitCode, stdout, stderr) <- readProcessWithExitCode cabal args ""

    case exitCode of
        ExitSuccess -> do
            putStrLn stdout
            return sourcePath
        ExitFailure _ -> do
            _ <- tryIOError (deletePkg repo pkg)
            ioError $ userError $ "Error downloading file: " ++ stderr
