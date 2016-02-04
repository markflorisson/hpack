{-# LANGUAGE RecordWildCards, NoMonomorphismRestriction #-}

module HPack.Infer.Inference
( inferIface )
where

import qualified Data.Set as S
import Data.Maybe (fromMaybe, maybe)
import Data.Map (Map, lookup, insert)
import Data.List (sort, sortBy)
import Control.Monad (forM_)

import HPack.Config (Config(..), CompilerVersion)
import HPack.Source (Pkg(..), ModulePath, SourceRepo)
import HPack.Cabal ( CabalRepo, CabalRepoM, CabalPkg(..), CabalError
                   , loadCabalFromPkg, runCabalRepoM)
import HPack.System (PkgDB, PkgId)
import HPack.Solver (SolverFlags(..), DepGraph, Disj(..), loadGraph, lookupPkg)
import HPack.Iface
    ( IfaceRepo, IfaceRepoM, runIfaceRepoM, IfaceRepoError
    , getPkgIface, addPkgIface
    , ExtractM, PkgInterface, runExtractM, extract
    )
import HPack.Monads

-- | Monad used in the inference process
type InferM m a = HPackT InferError State (IfaceRepoM m) a

data InferError
    = CabalError CabalError
    | BuildError String
    | InferenceError Pkg
    | PkgNotInGraph Pkg
    | IfaceRepoError IfaceRepoError

data State
    = State
        { cabalRepo     :: CabalRepo
        , sourceRepo    :: SourceRepo
        , ifaceRepoPath :: FilePath
        , pkgDB         :: PkgDB
        , config        :: Config
        , flags         :: SolverFlags
        }

-- | A particular package build configuration, indicating which versions
-- of the immediate package dependencies are to be chosen for some pkg
type BuildConfiguration = [Pkg]

-- | A possible plan to be proposed for compilation and interface extraction
data BuildPlan
    = BuildPlan
        { pkgToBuild :: Pkg
        , immediateDependencies :: [PkgId]
        , solverFlags :: SolverFlags
        }


-- | Run the intererence monad, returning an updated package and
-- interface repository
runInferM :: MonadIO m
          => InferM m a -> State
          -> m (Either InferError (a, PkgDB, IfaceRepo))
runInferM computation state = do
    eitherEitherVal <- runIface (runInfer computation)
    return $ do
        (eitherVal, ifaceRepo) <- eitherEitherVal
        (val, pkgdb) <- eitherVal
        return (val, pkgdb, ifaceRepo)
    where
        runInfer :: Monad m => InferM m a -> IfaceRepoM m (Either InferError (a, PkgDB))
        runInfer computation = flip runHPackT state $ do
            result <- computation
            state  <- get
            return (result, pkgDB state)

        runIface :: MonadIO m => IfaceRepoM m a -> m (Either InferError (a, IfaceRepo))
        runIface ifaceM = do
            eitherResult <- runIfaceRepoM (ifaceRepoPath state) ifaceM
            return (mapLeft IfaceRepoError eitherResult)


liftCabalRepoM :: Monad m => CabalRepoM m a -> InferM m a
liftCabalRepoM m = do
    result <- lift $ lift $ runCabalRepoM m
    tryEither $ mapLeft CabalError result

inferIface :: MonadIO m => Config -> Pkg -> InferM m PkgInterface
inferIface config pkg@(Pkg name version) = do
    State{..} <- get
    eitherGraph <- liftIO $ loadGraph cabalRepo config pkg
    depGraph <- tryEither $ mapLeft CabalError eitherGraph
    infer config depGraph pkg

-- | Find the possibilities for each dependency of a given package
findDeps :: Monad m => DepGraph -> Pkg -> InferM m [Disj]
findDeps depGraph pkg = tryMaybe (PkgNotInGraph pkg) (lookupPkg pkg depGraph)

-- | Infer a package interface for the given package, or raise an exception
-- in the InferM monad
--
-- TODO: keep a stack to guard against cyclic dependencies
--
infer :: MonadIO m => Config -> DepGraph -> Pkg -> InferM m PkgInterface
infer config depGraph pkg = findPkgIface $ do
        immediateDeps <- findDeps depGraph pkg

        -- build immediate dependencies
        forM_ immediateDeps $ \(Disj deps) ->
            mapM_ (infer config depGraph) deps

        -- generate all possible build configurations we can try randomly
        configurations <- randomConfigurations immediateDeps

        -- find the first one that works
        maybeConfiguration <- findFirstConfiguration configurations

        (firstConfiguration, firstIface) <-
            case maybeConfiguration of
                Just (c, iface) -> return (c, iface)
                Nothing         -> throw (InferenceError pkg)

        -- iteratively improve the build configuration by trying later
        -- dependency versions
        (buildConfig, pkgIface) <-
            findBestConfiguration firstConfiguration firstIface
                                  firstConfiguration immediateDeps

        return pkgIface
    where
        findPkgIface :: MonadIO m => InferM m PkgInterface -> InferM m PkgInterface
        findPkgIface computation = do
            maybePkgIface <- lift $ getPkgIface pkg
            case maybePkgIface of
                Just pkgIface -> return pkgIface
                Nothing       -> computation

        -- | Find the first configuration that actually builds properly
        findFirstConfiguration
            :: MonadIO m
            => [BuildConfiguration]
            -> InferM m (Maybe (BuildConfiguration, PkgInterface))
        findFirstConfiguration (c:cs) = do
            maybeIface <- build config depGraph pkg c
            case maybeIface of
                Just iface -> return (Just (c, iface))
                Nothing    -> findFirstConfiguration cs
        findFirstConfiguration [] = return Nothing

        -- | Find the "best" configuration by successively trying to build
        -- against the latest possible versions of a package's dependencies
        findBestConfiguration :: MonadIO m
                              => BuildConfiguration
                              -> PkgInterface
                              -> BuildConfiguration
                              -> [Disj]
                              -> InferM m (BuildConfiguration, PkgInterface)
        findBestConfiguration buildConfig iface (p:ps) (d:ds) = do
            let buildConfigs = improvedConfigurations buildConfig p d
            maybeCandidateConfig <- findFirstConfiguration buildConfigs
            let (buildConfig', iface') = fromMaybe (buildConfig, iface) maybeCandidateConfig
            findBestConfiguration buildConfig' iface' ps ds
        findBestConfiguration buildConfig iface [] [] = return (buildConfig, iface)

        -- | Get all possible build configurations in a random order
        randomConfigurations :: MonadIO m => [Disj] -> InferM m [BuildConfiguration]
        randomConfigurations disjs
            | [] <- disjs = return []
            | (Disj pkgs:disjs) <- disjs = do
                ps <- shuffle pkgs
                cs <- randomConfigurations disjs
                return [ pkg:comb | comb <- cs, pkg <- ps ]

        -- | Compute build configurations where 'pkg' in the given
        -- build configuration is replaced by later versions of itself
        improvedConfigurations
            :: BuildConfiguration -> Pkg -> Disj -> [BuildConfiguration]
        improvedConfigurations buildConfig p@(Pkg _ v) (Disj ps)
            = [ replacePkg p' buildConfig | p'@(Pkg _ v') <- ps, v' > v ]

        -- | Replace an older version of a package in a build configuration
        -- witha newer one
        replacePkg :: Pkg -> BuildConfiguration -> BuildConfiguration
        replacePkg pkg@(Pkg name version) pkgs
            = map replace pkgs
            where
                replace pkg'@(Pkg name' version')
                    | name == name' = pkg
                    | otherwise     = pkg'

-- | Build a package against the given versions of its immediate dependencies
build :: MonadIO m
      => Config
      -> DepGraph                       -- versioned-based dependency graph
      -> Pkg                            -- package to build
      -> BuildConfiguration             -- immediate dependencies
      -> InferM m (Maybe PkgInterface)
build config depGraph pkg buildConfiguration = do
      State{..} <- get
      plan <- computeBuildPlan pkg
      maybePkgId <- buildPackage plan
      case maybePkgId of
          Just pkgId -> extractPkgIface pkgId
          Nothing    -> return Nothing
    where
        computeBuildPlan :: MonadIO m => Pkg -> InferM m BuildPlan
        computeBuildPlan pkg = undefined

        buildPackage :: MonadIO m => BuildPlan -> InferM m (Maybe PkgId)
        buildPackage buildPlan = undefined

        extractPkgIface :: MonadIO m
                        => PkgId
                        -> InferM m (Maybe PkgInterface)
        extractPkgIface pkgId = do
            State{..} <- get
            cabalPkg <- liftCabalRepoM $ loadCabalFromPkg cabalRepo config pkg
            eitherPkgIface <- lift $ runExtractM $ extract pkgDB pkgId cabalPkg
            case eitherPkgIface of
                Left err       -> return Nothing
                Right pkgIface -> return (Just pkgIface)
