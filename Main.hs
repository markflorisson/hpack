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

type GhcE = ExceptT Err Ghc

printErr :: Err -> Ghc ()
printErr (CompileErr m) =
    liftIO $ putStrLn $ "Failed to compile " ++ m
printErr (IfaceErr m) =
    liftIO $ putStrLn $ "Failed to load interface for " ++ m

main :: IO ()
main = defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
        result <- runExceptT compileAndLoad
        case result of
            Left err -> printErr err
            Right () -> liftIO $ putStrLn "All done."
    where
        compileAndLoad :: ExceptT Err Ghc ()
        compileAndLoad = do
            let modName = "Mod"
            mainCompile modName
            mainLoadIface modName

mainLoadIface :: String -> GhcE ()
mainLoadIface modName = do
    maybeIface <- lift $ loadIface "Mod"
    dynflags <- lift getDynFlags
    case maybeIface of
        Just iface -> liftIO $ putStrLn $
            "Loaded interface " ++ modName ++ ":\n" ++
            showModIface dynflags iface
        Nothing    -> throwE $ IfaceErr modName

loadIface :: String-> Ghc (Maybe ModIface)
loadIface modName = do
    mod <- findModule (mkModuleName modName) Nothing
    maybeModInfo <- getModuleInfo mod
    return $ modInfoIface =<< maybeModInfo

mainCompile :: String -> GhcE ()
mainCompile modName = do
    result <- lift $ compileTarget modName
    case result of
        Succeeded -> liftIO $ putStrLn $ "Successfully compiled " ++ modName
        Failed    -> throwE $ CompileErr modName

compileTarget :: String -> Ghc SuccessFlag
compileTarget modName = do
    -- dflags <- getSessionDynFlags
    -- setSessionDynFlags dflags
    initDynFlags
    target <- guessTarget modName Nothing
    setTargets [target]
    load LoadAllTargets

-----------

showModIface :: DynFlags -> ModIface -> String
showModIface dynflags = showSDoc dynflags . pprModIface

initDynFlags :: Ghc ()
initDynFlags = void $ setSessionDynFlags =<< getDynFlags

getDynFlags :: Ghc DynFlags
getDynFlags = getSessionDynFlags

-----------

ghcShow :: Outputable a => a -> Ghc String
ghcShow x = do
    dynflags <- getDynFlags
    return $ showSDoc dynflags $ ppr x
