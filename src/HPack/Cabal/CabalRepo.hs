module HPack.Cabal.CabalRepo
( CabalRepo(..), CabalError(..)
, CabalRepoM, runCabalRepoM
, openCabalRepo, loadCabalFromPkg
, listCabalPkgs
) where

import Text.ParserCombinators.ReadP (readP_to_S)
import Data.Version (Version, parseVersion)
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.IO.Error (tryIOError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Maybe (mapMaybe)

import HPack.Source (Pkg(..), ModulePath(..), Version, showVersion)
import HPack.Cabal.Cabal (CabalPkg(..), CompilerVersion, loadCabalFromFile)
import HPack.Config (Config(..))
import HPack.Utils (whenM, raiseEither)

data CabalError
    = CabalRepoNotFound FilePath
    | CabalFileNotFound Pkg
    | CabalParseError Pkg String
    | CabalIOError IOError

-- | Repository of cabal metadata for all packages downloaded from
-- https://hackage.haskell.org/01-index.tar.gz
data CabalRepo
    = CabalRepo { path :: FilePath }

type CabalRepoM = ExceptT CabalError

runCabalRepoM :: CabalRepoM m a -> m (Either CabalError a)
runCabalRepoM = runExceptT

---------------------------------------------------------

openCabalRepo :: MonadIO m => FilePath -> CabalRepoM m CabalRepo
openCabalRepo path = do
    whenM (liftIO $ not <$> doesDirectoryExist path) $
        throwE $ CabalRepoNotFound path
    return $ CabalRepo path

loadCabalFromPkg :: MonadIO m => CabalRepo -> Config -> Pkg
                 -> CabalRepoM m CabalPkg
loadCabalFromPkg repo config pkg = do
    let fn = getCabalFilename repo pkg
    eitherCabalPkg <- liftIO $ tryIOError $ loadCabalFromFile config fn
    raiseEither CabalIOError eitherCabalPkg

getCabalFilename :: CabalRepo -> Pkg -> FilePath
getCabalFilename (CabalRepo path) (Pkg name version)
    = path </> "metadata" </> name </> showVersion version </> (name ++ ".cabal")

listCabalPkgs :: CabalRepo -> String -> IO [Pkg]
listCabalPkgs (CabalRepo path) pkgName = do
    versionStrings <- getDirectoryContents (path </> "metadata" </> pkgName)
    let versions = mapMaybe readVersion versionStrings
    return (map (Pkg pkgName) versions)

readVersion :: String -> Maybe Version
readVersion versionString =
    case readP_to_S parseVersion versionString of
        (version, _):xs -> Just version
        _            -> Nothing
