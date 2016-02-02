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
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import qualified Data.ByteString.Lazy as BS

import HPack.JSON
import HPack.Monads
import HPack.Config (Config(..))
import HPack.Cabal (CabalPkg(..), loadCabalFromFile, writeCabalFile, fixPackageVersions)
import HPack.Source.SourceRepo (SourceRepo, getSourceForPkg)
import HPack.Source.Package (Pkg(..), Version(..), ModulePath, showVersion)
import HPack.Utils (whenM, raiseEither)

data DBErr
    = DBIOError IOError
        -- ^ some IOError has occurred
    | DBSourceError IOError
        -- ^ Error downloading package sources
    | DBPkgNotFound PkgId
        -- ^ the package ID is not valid
    | MalformedJSON FilePath
        -- ^ malformatted JSON for package database state

type DB m = HPackT DBErr PkgDB m

-- | Database of packages that are compiled on the system
data PkgDB = PkgDB
    { dbLocation :: FilePath
        -- ^ package db location on disk
    , pkgInfo    :: M.Map PkgId Pkg
        -- ^ package name and version for each package ID
    , depInfo    :: M.Map PkgId [PkgId]
        -- ^ dependencies for each package ID
    , renamedPkgVersions :: M.Map PkgId Version
        -- ^ renamed version so that we can compile the same package
        -- version against different versions of its dependencies.
        -- For example, we may link Foo-0.1 against Text-0.1
        -- Text-0.2, but the GHC package database does not support this.
        -- So instead we register the packages under some random version,
        -- e.g. Foo-0.1.0.0.0.1928291 and Foo-0.1.0.0.0.473729
    }
    deriving (Show, Generic)

-- | A concrete package *instantiation*: that is, a particular
-- compilation and linkage of a package. Also used to store the
-- package on disk in the PkgDB
type PkgId = Version

instance ToJSON PkgDB
instance FromJSON PkgDB

io :: MonadIO m => IO a -> DB m a
io = tryIO DBIOError

---------------------------------------------------------

-- | Create initial (empty) package database
runDB :: MonadIO m => FilePath -> DB m a -> m (Either DBErr (a, PkgDB))
runDB path computation
    | initialPkgDB <- PkgDB path M.empty M.empty M.empty
    = flip runHPackT initialPkgDB $ do
        io $ createDirectoryIfMissing False path
        loadPkgDB
        val <- computation
        savePkgDB
        pkgDB <- get
        return (val, pkgDB)

-- | Load the package dependency graph from a JSON file
loadPkgDB :: MonadIO m => DB m ()
loadPkgDB = do
    dbPath <- liftM dbLocation get
    filename <- getJSONFileName
    haveFile <- io $ doesFileExist filename
    if not haveFile
        then return ()
        else do contents <- io $ BS.readFile filename
                case decode contents of
                    Just pkgDB ->
                        -- keep the path from the new location if the
                        -- repository has been relocated on the filesystem
                        put $ pkgDB { dbLocation = dbPath }
                    Nothing ->
                        throw (MalformedJSON filename)

-- | Save package database to disk (to dbLocation)
savePkgDB :: MonadIO m => DB m ()
savePkgDB = do
    filename <- getJSONFileName
    pkgDB <- get
    io $ BS.writeFile filename $ encode pkgDB

getJSONFileName :: Monad m => DB m FilePath
getJSONFileName = do
    dbPath <- liftM dbLocation get
    return $ dbPath ++ "dependencyGraph.json"

---------------------------------------------------------

getFilesAndFolders :: FilePath -> IO [FilePath]
getFilesAndFolders path =
    filter (`notElem` [".", ".."]) <$> getDirectoryContents path

-- | `cloneDir src dest` clones the directory at the location pointed at
--   by `src` to `dest`.
cloneDir :: FilePath -> FilePath -> IO ()
cloneDir src dest = do
    -- get the contents of the source directory and create the
    -- target directory, if required
    createDirectoryIfMissing False dest
    fs <- getFilesAndFolders src

    -- copy all the files and folders
    forM_ fs $ \name -> do
        let srcPath = src </> name
        let dstPath = dest </> name

        isDir <- doesDirectoryExist srcPath

        if isDir
        then cloneDir srcPath dstPath
        else copyFile srcPath dstPath

-- | Try to compile the package against the given dependencies.
-- On success, returns a new PkgId for the package, and update the
-- PkgDB
tryCompile :: MonadIO m
           => Config
           -> SourceRepo
           -> Pkg
           -> [PkgId]
           -> DB m (SourceRepo, PkgId)
tryCompile config srcRepo pkg@(Pkg name version) dependencies = do
        PkgDB {..} <- get
        pkgDB      <- get

        -- 0) Download package sources
        (srcRepo', pkgLocation) <- io (getSourceForPkg srcRepo pkg)

        -- 1) allocate build directory in package repo:
        --
        --      pkgDB/my-pkg/0.1.0.0.0.1829373
        --
        pkgId <- freshVersion version
        let buildDir = getBuildDir pkgDB pkgId
        io $ createDirectoryIfMissing True buildDir

        -- 2) copy package sources into build directory
        tryIO DBIOError $ cloneDir pkgLocation buildDir

        -- 3) rewrite cabal file to build package against fixed versions of
        -- dependencies
        updateCabalFile buildDir

        -- 4) cabal build the package
        cabalBuild buildDir

        -- 5) if successful, update PkgDB
        put PkgDB { dbLocation = dbLocation
                  , pkgInfo = M.insert pkgId pkg pkgInfo
                  , depInfo = M.insert pkgId dependencies depInfo
                  , renamedPkgVersions = renamedPkgVersions
                  }

        return (srcRepo', pkgId)
    where
        -- Replace cabal file with fixed package versions, for example
        --
        --      build-depends:
        --          foo == 1.4.2.0.0.0.283928,
        --          ...
        --
        updateCabalFile :: MonadIO m => FilePath -> DB m ()
        updateCabalFile buildDir = do
            let cabalFile = buildDir </> name ++ ".cabal"
            (cabalPkg, packageDesc) <- io $ loadCabalFromFile config cabalFile
            fixedPkgVersions <- mapM lookupRenamedPkg' dependencies
            let modifiedPkgDesc = fixPackageVersions fixedPkgVersions packageDesc
            io $ writeCabalFile cabalFile modifiedPkgDesc

        -- | Try building a version-adjusted package
        cabalBuild :: MonadIO m => FilePath -> DB m ()
        cabalBuild buildDir = do
            let cabal = cabalPath config
            io $ invokeProcess cabal
                ["sandbox", "init", "--sandbox=../../.cabal-sandbox"]
            io $ invokeProcess cabal ["configure"]
            io $ invokeProcess cabal ["build"]
            io $ invokeProcess cabal ["install"]
            return ()

        -- | Invoke a process and return its output
        --
        -- TODO: Capture stdout and stderr on the same stream to avoid reordering
        --       (preferably on a pty)
        --
        invokeProcess :: String -> [String] -> IO String
        invokeProcess cmd args = do
            (exitCode, stdout, stderr) <- readProcessWithExitCode cmd args ""
            case exitCode of
                ExitSuccess          -> return (stdout ++ stderr)
                ExitFailure exitCode -> fail   (stdout ++ stderr)


-- | Choose a fresh package version that is unlikely to conflict with
-- any previously chosen version
-- For debuggability base the version on an actual package version
freshVersion :: MonadIO m => Version -> DB m Version
freshVersion (Version branch tags) = do
    versionExtension <- forM [1..10] $ \_ -> random (0, 1000000000)
    return (Version (branch ++ [0, 0, 0] ++ versionExtension) tags)

-- | Find all existing compilations in the package database for the
-- given package (useful to figure out what compilations to propose)
findCompilations :: PkgDB -> Pkg -> [PkgId]
findCompilations pkgDB pkg =
    [ pkgId | (pkgId, pkg') <- M.assocs (pkgInfo pkgDB), pkg == pkg' ]

-- | Lookup the package with the renamed version for a given PkgId
--   Raise an exception if not found.
lookupRenamedPkg' :: Monad m => PkgId -> DB m Pkg
lookupRenamedPkg' pkgId = do
    pkgDB <- get
    tryMaybe (DBPkgNotFound pkgId) (lookupRenamedPkg pkgDB pkgId)

-- | Lookup the package with the renamed version for a given PkgId
lookupRenamedPkg :: PkgDB -> PkgId -> Maybe Pkg
lookupRenamedPkg PkgDB{..} pkgId
    | Just (Pkg name _) <- M.lookup pkgId pkgInfo
        -- ^ the package with the original version
    , Just version      <- M.lookup pkgId renamedPkgVersions
        -- ^ the version as renamed by 'freshVersion' for the built package
        = Just (Pkg name version)
    | otherwise
        = Nothing

lookupPkg :: PkgDB -> PkgId -> Maybe Pkg
lookupPkg PkgDB{..} pkgId = M.lookup pkgId pkgInfo

getBuildDir :: PkgDB -> PkgId -> FilePath
getBuildDir PkgDB{..} pkgId
    | Just (Pkg name _) <- M.lookup pkgId pkgInfo
        = dbLocation </> name </> showVersion pkgId
