{-# LANGUAGE RecordWildCards #-}

module HPack.Iface.IfaceExtract
( ExtractM, runExtractM, extract )
where

import GHC
    ( getSessionDynFlags, setSessionDynFlags
    , runGhc, Ghc, GhcMonad
    , defaultErrorHandler, SuccessFlag(..)
    , ModIface(..), mkModuleName, findModule, getModuleInfo, modInfoIface
    , guessTarget, setTargets, LoadHowMuch(..), load
    )
-- compiler/basicTypes/Avail.hs
import Avail (AvailInfo, availName)
    -- ^ be careful with AvailInfo, as its signature does not seem to
    -- be stable. Instead use 'availName' etc
-- compiler/utils/Outputable.hs
import Outputable (SDoc, ppr, text, (<>), (<+>))
import GHC.Fingerprint (Fingerprint)
import GHC.Generics
import IfaceSyn
import OccName

import Data.String.Utils (split)
import qualified Data.Map as M
import qualified Data.Set as S
import System.FilePath ((</>))

import HPack.Iface.Iface
import HPack.Iface.IfaceRepo
import HPack.Iface.LoadIface
import HPack.Cabal (CabalPkg)
import HPack.Source (Pkg(..), ModulePath(..))
import HPack.System (PkgDB, PkgId, getBuildDir, lookupPkg)
import HPack.Ghc (SDoc, ghcShow, ghcShowSDoc)
import HPack.Monads
import HPack.JSON

type ExtractM = IfaceRepoM IfaceM

runExtractM :: ExtractM a -> IO (Either Err a)
runExtractM = undefined

extract :: PkgDB -> PkgId -> ExtractM PkgInterface
extract pkgDB pkgId = do
    let buildDir = getBuildDir pkgDB pkgId
    let dirWithCompiledFiles = buildDir </> "dist" </> "build"
    let modNames = undefined
    let Just (Pkg name version) = lookupPkg pkgDB pkgId
    modIfaces <-
        forM modNames $ \modName -> do
            modIface <- lift $ compileAndLoadIface dirWithCompiledFiles modName
            liftIO $ extractIface modIface
    let modPaths = map modulePath modNames
    return $ PkgInterface name version (M.fromList (zip modPaths modIfaces))

modulePath :: String -> ModulePath
modulePath = ModulePath . split "."

extractIface :: ModIface -> IO ModInterface
extractIface modIface = undefined

extractDecl :: IfaceDecl -> Maybe Symbol
extractDecl IfaceId{..}
    | name     <- extractName ifName
    , origin   <- extractOrigin ifName
    , typ      <- extractType ifType
    = Just (Fun name origin typ)
extractDecl IfaceData{..}
    | name     <- extractName ifName
    , origin   <- extractOrigin ifName
    -- , kind     <- extractKind ifKind
        -- ^ this field does not exist in ghc 7.10
    , tyvars   <- map extractTypeVar ifTyVars
    , datacons <- extractDataCons ifCons
    -- NOTE: record fields are already exported as functions (IfaceId)
    = Just (DataType name origin tyvars datacons)
extractDecl IfaceSynonym{..}
    | name     <- extractName ifName
    , origin   <- extractOrigin ifName
    , kind     <- extractKind ifSynKind
    , tyvars   <- map extractTypeVar ifTyVars
    , rhs      <- extractType ifSynRhs
    = Just (TypeSynonym name origin kind tyvars rhs)
extractDecl IfaceClass{..}
    | name     <- extractName ifName
    , origin   <- extractOrigin ifName
    , sigs     <- S.fromList (map extractMethodSignature ifSigs)
    = Just (ClassDef name origin sigs)
extractDecl IfaceFamily{..}
    = Nothing -- TODO
extractDecl IfaceAxiom{..}
    = Nothing -- TODO
extractDecl IfacePatSyn{..}
    = Nothing -- TODO

extractName :: OccName -> String
extractName = occNameString

extractOrigin :: OccName -> Origin
extractOrigin = undefined

extractType :: IfaceType -> Type
extractType = undefined

extractTypeVar :: IfaceTvBndr -> TypeVar
extractTypeVar = undefined

extractKind :: IfaceKind -> Kind
extractKind = undefined

extractDataCons :: IfaceConDecls -> [DataCon]
extractDataCons = undefined

extractDataCon :: IfaceConDecl -> DataCon
extractDataCon = undefined

extractMethodSignature :: IfaceClassOp -> Symbol
extractMethodSignature = undefined
