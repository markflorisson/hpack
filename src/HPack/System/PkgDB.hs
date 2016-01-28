{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module HPack.System.PkgDB

where

import qualified Data.Map as M
import GHC.Generics

import System.Directory
    ( createDirectoryIfMissing, getDirectoryContents
    , doesDirectoryExist, doesFileExist, copyFile)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import Control.Monad
import qualified Data.ByteString.Lazy as BS

import HPack.JSON
import HPack.Monads
import HPack.Source.SourceRepo (SourceRepo, getSourceForPkg)
import HPack.Source.Package (Pkg(..), ModulePath)
import HPack.Utils (whenM, raiseEither)

data DBErr
    -- | some IOError has occurred
    = DBInitError IOError
    -- | Error downloading package sources
    | SourceDownloadError IOError
    -- | the sources for some package couldn't be found
    | DBPkgNotFound
    | MalformedJSON FilePath

type DB m = HPackT DBErr PkgDB m

-- | Database of packages that are compiled on the system
data PkgDB = PkgDB
    { dbLocation :: FilePath
        -- ^ package db location on disk
    , pkgInfo    :: M.Map PkgId Pkg
        -- ^ package name and version for each package ID
    , depInfo    :: M.Map PkgId [PkgId]
        -- ^ dependencies for each package ID
    }
    deriving (Show, Generic)

-- | A concrete package *instantiation*: that is, a particular
-- compilation and linkage of a package. Also used to store the
-- package on disk in the PkgDB
newtype PkgId = PkgId Int
    -- this should probably not be an int but a SHA256 hash or something
    deriving (Ord, Eq, Show, Generic)

instance ToJSON PkgDB
instance FromJSON PkgDB

instance ToJSON PkgId
instance FromJSON PkgId

---------------------------------------------------------

-- | Create initial (empty) package database
runDB :: MonadIO m => FilePath -> DB m a -> m (Either DBErr (a, PkgDB))
runDB path computation = flip runHPackT (PkgDB path M.empty M.empty) $ do
    liftIO $ createDirectoryIfMissing False path
    loadPkgDB
    val <- computation
    savePkgDB
    pkgDB <- get
    return (val, pkgDB)

-- | Load the package dependency graph from a JSON file
loadPkgDB :: MonadIO m => DB m ()
loadPkgDB = do
    dbPath <- liftM dbLocation get
    let filename = dbPath ++ "dependencyGraph.json"
    haveFile <- liftIO $ doesFileExist filename
    if not haveFile
        then return ()
        else do contents <- liftIO $ BS.readFile filename
                case decode contents of
                    Just (pkgInfo, depInfo) ->
                        put $ PkgDB dbPath pkgInfo depInfo
                    Nothing ->
                        throw (MalformedJSON filename)

-- | Save package database to disk (to dbLocation)
savePkgDB :: MonadIO m => DB m ()
savePkgDB = undefined

---------------------------------------------------------

getFilesAndFolders :: FilePath -> IO [FilePath]
getFilesAndFolders path =
    filter (`notElem` [".", ".."]) <$> getDirectoryContents path

-- | `cloneDir src dest` clones the directory at the location pointed at
--   by `src` to `dest`.
cloneDir :: MonadIO m => FilePath -> FilePath -> DB m ()
cloneDir src dest = do
    -- the source directory must exist
    whenM (liftIO $ not <$> doesDirectoryExist src) $
        throw DBPkgNotFound

    -- get the contents of the source directory and create the
    -- target directory, if required
    fs <- liftIO $ do
        createDirectoryIfMissing False dest

        -- list the contents of the directory
        getFilesAndFolders src

    -- copy all the files and folders
    forM_ fs $ \name -> do
        let srcPath = src </> name
        let dstPath = dest </> name

        isDir <- liftIO $ doesDirectoryExist srcPath

        if isDir
        then cloneDir srcPath dstPath
        else liftIO $ copyFile srcPath dstPath

-- | Try to compile the package against the given dependencies.
-- On success, returns a new PkgId for the package, and update the
-- PkgDB
tryCompile :: MonadIO m
           => SourceRepo
           -> Pkg
           -> [PkgId]
           -> DB m (SourceRepo, PkgId)
tryCompile srcRepo pkg dependencies = do
    -- 0) Download package sources
    (srcRepo', pkgLocation) <- liftIO (getSourceForPkg srcRepo pkg)

    -- 1) allocate temporary directory
    -- TODO: where should we create the temporary directory?
    tmpDir <- liftIO $ createTempDirectory "." "hpack"

    -- 2) copy package sources into temporary directory
    cloneDir pkgLocation tmpDir

    -- 3) replace cabal file with specific package versions to match
    --    the versions from Candidate
    --    (this may be the easiest way to "fix" the versions of the
    --     dependencies, but maybe there is another way)

    -- 4) cabal build the package

    -- 5) compute package hash
    pkgId <- computePkgHash pkg dependencies

    -- 6) move tmpDir to dbLocation/pkgId
    --    (move package under its proper hash in the package database)

    -- 7) if successful, update PkgDB
    PkgDB {..} <- get
    let pkgInfo' = M.insert pkgId pkg pkgInfo
    let depInfo' = M.insert pkgId dependencies depInfo
    modify (\pkgDB -> PkgDB dbLocation pkgInfo' depInfo')

    -- 8) register the compiled package with GHC's database
    -- ghc-pkg register / cabal install

    -- 9) done :)
    return (srcRepo', pkgId)

-- | Compute the hash for the package, given its name, version and
-- a list of its compiled dependencies
computePkgHash :: MonadIO m => Pkg -> [PkgId] -> DB m PkgId
computePkgHash pkg dependencies = undefined

-- | Find all existing compilations in the package database for the
-- given package (useful to figure out what compilations to propose)
findCompilations :: PkgDB -> Pkg -> [PkgId]
findCompilations = undefined
