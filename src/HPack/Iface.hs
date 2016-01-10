module HPack.Iface
( IfaceM
, Err(..)
, ModName
, ModIface
, runIfaceM
, mainCompile
, mainLoadIface
, compileAndLoadIface
, showModIface
)
where

import GHC
    ( getSessionDynFlags, setSessionDynFlags
    , runGhc, Ghc, GhcMonad
    , defaultErrorHandler, SuccessFlag(..)
    , ModIface, mkModuleName, findModule, getModuleInfo, modInfoIface
    , guessTarget, setTargets, LoadHowMuch(..), load
    )
import GHC.Paths (libdir)
import LoadIface (pprModIface)
import DynFlags (DynFlags, defaultFatalMessager, defaultFlushOut)
import Outputable (Outputable(..), showSDoc, ppr)

import Control.Monad (liftM, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, catchE)
import Control.Monad.Trans.Class (lift)

type ModName = String

data Err
    = CompileErr ModName
    | IfaceErr ModName

type IfaceM = ExceptT Err Ghc

-- | Run the IfaceM monad
runIfaceM :: IfaceM a -> IO (Either Err a)
runIfaceM = defaultErrorHandler defaultFatalMessager defaultFlushOut
          . runGhc (Just libdir)
          . runExceptT

-- | First compile the package, then load the interface
compileAndLoadIface :: ModName -> IfaceM ModIface
compileAndLoadIface modName
    = mainCompile modName >> mainLoadIface modName

---------------------------------------------------------

-- | Compile the given module name
-- TODO: We may want to replace this by a 'cabal build' instead
mainCompile :: ModName -> IfaceM ()
mainCompile modName = do
    result <- lift $ compileTarget modName
    case result of
        Succeeded -> return ()
        Failed    -> throwE $ CompileErr modName

compileTarget :: ModName -> Ghc SuccessFlag
compileTarget modName = do
    -- dflags <- getSessionDynFlags
    -- setSessionDynFlags dflags
    initDynFlags
    target <- guessTarget modName Nothing
    setTargets [target]
    load LoadAllTargets

---------------------------------------------------------

-- | Load the interface information for the given module
mainLoadIface :: ModName -> IfaceM ModIface
mainLoadIface modName = do
    maybeIface <- lift $ loadIface modName
    case maybeIface of
        Just iface -> return iface
        Nothing    -> throwE $ IfaceErr modName

loadIface :: ModName -> Ghc (Maybe ModIface)
loadIface modName = do
    -- find the module loaded through compilation
    mod <- findModule (mkModuleName modName) Nothing
    -- extract module interface information
    maybeModInfo <- getModuleInfo mod
    return $ modInfoIface =<< maybeModInfo

---------------------------------------------------------

initDynFlags :: Ghc ()
initDynFlags = void $ setSessionDynFlags =<< getDynFlags

getDynFlags :: Ghc DynFlags
getDynFlags = getSessionDynFlags

---------------------------------------------------------

ghcShow :: Outputable a => a -> Ghc String
ghcShow x = do
    dynflags <- getDynFlags
    return $ showSDoc dynflags $ ppr x

showModIface :: ModIface -> IfaceM String
showModIface modIface = lift $ do
    dynflags <- getDynFlags
    return $ showSDoc dynflags $ pprModIface modIface

instance Show Err where
    show (CompileErr m) = "Failed to compile " ++ m ++ "."
    show (IfaceErr m)   = "Failed to load interface for " ++ m ++ "."
