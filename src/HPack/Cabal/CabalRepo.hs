module HPack.Cabal.CabalRepo
( CabalRepo(..), CabalError(..)
, CabalRepoM, runCabalRepoM
, openCabalRepo, loadCabalFromPkg
) where

import System.FilePath ((</>))
import System.Directory (doesDirectoryExist)
import System.IO.Error (tryIOError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

import HPack.Source (Pkg(..), ModulePath(..), Version, showVersion)
import HPack.Cabal.Cabal (CabalPkg(..), CompilerVersion, loadCabalFromFile)
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

loadCabalFromPkg :: MonadIO m => CabalRepo -> CompilerVersion -> Pkg
                 -> CabalRepoM m CabalPkg
loadCabalFromPkg repo version pkg = do
    let fn = getCabalFilename repo pkg
    eitherCabalPkg <- liftIO $ tryIOError $ loadCabalFromFile version fn
    raiseEither CabalIOError eitherCabalPkg

getCabalFilename :: CabalRepo -> Pkg -> FilePath
getCabalFilename (CabalRepo path) (Pkg name version)
    = "metadata" </> name </> showVersion version </> (name ++ ".cabal")
