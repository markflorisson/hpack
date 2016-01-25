{-# LANGUAGE RecordWildCards #-}

module HPack.Infer.Inference
( inferIface )
where

import Data.Map (Map, lookup, insert)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, runStateT, put, get, modify)
import Control.Monad.Trans.Except
    (ExceptT, runExceptT, throwE, catchE, withExceptT)

import HPack.Config (Config(..), CompilerVersion)
import HPack.Source (Pkg(..), ModulePath, SourceRepo)
import HPack.Cabal (CabalRepo, CabalPkg, CabalError, loadCabalFromPkg)
import HPack.System (PkgDB, PkgId)
import HPack.Infer.IfaceRepo (IfaceRepo)
import HPack.Infer.IfaceExtract (PkgInterface, extractPkgInterface)

data InferError
    = CabalError CabalError
    | BuildError String
    | InferenceError Pkg

data State
    = State
        { cabalRepo     :: CabalRepo
        , sourceRepo    :: SourceRepo
        , pkgDB         :: PkgDB
        , ifaceRepo     :: IfaceRepo
        }

type InferM m a = ExceptT InferError (StateT State m) a

inferIface :: MonadIO m => Config -> Pkg -> InferM m PkgInterface
inferIface config pkg@(Pkg name version) = do
    State{..} <- lift get
    cabalPkg <- withExceptT CabalError $ loadCabalFromPkg cabalRepo config pkg

    let pkdId = undefined
    liftIO $ extractPkgInterface pkgDB pkdId cabalPkg
