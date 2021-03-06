{- | Use GHC to load a ModIface from a .hi file -}
module HPack.Iface.LoadIface
( IfaceM
, ModName(..)
, ModIface
, runIfaceM
, compileAndLoadIface
, showModIface
) where

import GHC
    ( getSessionDynFlags, setSessionDynFlags, workingDirectoryChanged
    , runGhc, Ghc, GhcMonad
    , defaultErrorHandler, SuccessFlag(..)
    , ModIface, mkModuleName, findModule, getModuleInfo, modInfoIface
    , guessTarget, setTargets, LoadHowMuch(..), load
    )
import GHC.Paths (libdir)
import LoadIface (pprModIface)
import BinIface (readBinIface, CheckHiWay(..), TraceBinIFaceReading(..))
import TcRnMonad (initTcRnIf)
import HscTypes (HscEnv)
import HscMain (newHscEnv)
import DynFlags (DynFlags, defaultFatalMessager, defaultFlushOut)
import Outputable (Outputable(..), showSDoc, ppr)

import System.Directory (setCurrentDirectory)
import System.FilePath ((</>))
import Control.Monad (liftM, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, catchE)
import Control.Monad.Trans.Class (lift)

import HPack.Ghc
import HPack.Source (Pkg(..))

type ModName = String

type IfaceM = Ghc

-- | Run the IfaceM monad
runIfaceM :: IfaceM a -> IO a
runIfaceM = defaultErrorHandler defaultFatalMessager defaultFlushOut
          . runGhc (Just libdir)

-- | First compile the package, then load the interface
compileAndLoadIface :: FilePath -> ModName -> IfaceM ModIface
compileAndLoadIface buildDir modName = do
    liftIO $ putStrLn $ "Loading module: " ++ show modName
    chdir buildDir
    initDynFlags
    loadIface modName

-- | Change the working directory and inform GHC
chdir :: FilePath -> IfaceM ()
chdir path = do
    liftIO (setCurrentDirectory path)
    workingDirectoryChanged


-- | Load the interface information for the given module
loadIface :: ModName -> IfaceM ModIface
loadIface modName = do
    let filePath = haskellInterfacePath modName
    dynflags <- getSessionDynFlags
    liftIO $ readIface dynflags filePath
    where
        -- | Read binary interface (adapted from 'showIface' in GHC's
        --      iface/LoadIface.hs)
        readIface :: DynFlags -> FilePath -> IO ModIface
        readIface dynflags filename = do
            hscEnv <- newHscEnv dynflags
            initTcRnIf 's' hscEnv () () $
                readBinIface IgnoreHiWay TraceBinIFaceReading filename

-- | Determine the path to the .hi file for a given module name "Foo.Bar.Baz"
haskellInterfacePath :: ModName -> FilePath
haskellInterfacePath modName =
    [ if c == '.' then '/' else c | c <- modName ] ++ ".hi"

---------------------------------------------------------

showModIface :: ModIface -> IfaceM String
showModIface modIface = ghcShowSDoc $ pprModIface modIface
