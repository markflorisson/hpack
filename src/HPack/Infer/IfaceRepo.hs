module HPack.Infer.IfaceRepo
( IfaceRepo(..), IfaceRepoM, IfaceRepoError(..)
, runIfaceRepoM, addPkgIface, getPkgIface
) where

import qualified Data.Map as M
import System.FilePath ((</>))
import System.Directory
    ( doesFileExist, doesDirectoryExist, getDirectoryContents
    , createDirectoryIfMissing, removeDirectoryRecursive)
import qualified Data.ByteString.Lazy as BS

import HPack.Monads
import HPack.System (PkgId(..))
import HPack.Source (Pkg(..), ModulePath(..), showVersion)
import HPack.Infer.IfaceExtract
    (PkgInterface(..), ModInterface(..), Symbol(..), Origin(..))
import HPack.JSON

type IfaceRepoM m = HPackT IfaceRepoError IfaceRepo m

-- | Repository for interfaces inferred from packages. This is needed for
-- the HPack solver to compute package versions
data IfaceRepo
    = IfaceRepo
        { path :: FilePath
            -- ^ path to the on-disk repository
            -- The repository is a directory containing
            -- subdirectories of the form 'mypkg/0.2.3/interface.json'
        , interfaces :: M.Map Pkg PkgInterface
            -- ^ In-memory cache of the interface repository, loaded
            -- lazily from disk as necessary
        }

data IfaceRepoError
    = IfaceNotFound Pkg
    | IfaceLoadError IOError
    | IfaceParseError FilePath

-- | Run the interface monad given the directory path to the repository
runIfaceRepoM :: MonadIO m
              => FilePath
              -> IfaceRepoM m a
              -> m (Either IfaceRepoError (a, IfaceRepo))
runIfaceRepoM path computation = flip runHPackT (IfaceRepo "" M.empty) $ do
    liftIO $ createDirectoryIfMissing False path
    put $ IfaceRepo path M.empty
    val <- computation
    ifaceRepo <- get
    return (val, ifaceRepo)

-- | Add a package interface, updating the filesystem and the
-- in-memory cache
addPkgIface :: MonadIO m => Pkg -> PkgInterface -> IfaceRepoM m ()
addPkgIface pkg@(Pkg name version) pkgInterface = do
    ifaceRepo <- get
    let dirname = ifacePath ifaceRepo pkg
    liftIO $ createDirectoryIfMissing True dirname
    liftIO $ BS.writeFile (ifaceFile ifaceRepo pkg) (encode pkgInterface)
    return ()

-- | Get the interface of a package if available in the repository, and add
-- it to the in-memory cache
getPkgIface :: MonadIO m => Pkg -> IfaceRepoM m (Maybe PkgInterface)
getPkgIface pkg = do
    ifaceRepo <- get
    let fileName = ifaceFile ifaceRepo pkg
    haveFile <- liftIO $ doesFileExist fileName
    if haveFile
        then do
            pkgIface <- loadFile fileName
            IfaceRepo path ifaces <- get
            put $ IfaceRepo path (M.insert pkg pkgIface ifaces)
            return (Just pkgIface)
        else return Nothing
    where
        loadFile :: MonadIO m => FilePath -> IfaceRepoM m PkgInterface
        loadFile fileName = do
            contents <- liftIO $ BS.readFile fileName
            case decode contents of
                Just pkgInterface -> return pkgInterface
                Nothing           -> throw $ IfaceParseError fileName

-- | Get the path to a particular package interface direcotry
ifacePath :: IfaceRepo -> Pkg -> FilePath
ifacePath (IfaceRepo path _) (Pkg name version)
    = path </> name </> showVersion version

-- | Get the path to a JSON interface file for the given package
ifaceFile :: IfaceRepo -> Pkg -> FilePath
ifaceFile ifaceRepo pkg = ifacePath ifaceRepo pkg </> "interface.json"
