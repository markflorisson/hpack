{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

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
import IfaceType
import OccName
import Avail
import FastString (FastString, unpackFS)
import qualified Name as GHC

import Data.Maybe (mapMaybe, catMaybes)
import System.FilePath ((</>))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy      as BS
import qualified Control.Monad.Trans.Class as Monad
import qualified Control.Monad.IO.Class    as Monad

import HPack.Iface.Iface
import HPack.Iface.IfaceRepo
import HPack.Iface.LoadIface
import HPack.Iface.ExternalSymbols
    ( ModuleRequirements(..), RequiredSymbolName(..)
    , PkgOrigin(..), parseSymbolsForModule)
import HPack.Cabal (CabalPkg(..))
import HPack.Source (Pkg(..), ModulePath(..), mkModulePath)
import HPack.System (PkgDB, PkgId, getBuildDir, lookupPkg)
import HPack.Ghc (SDoc, ghcShow, ghcShowSDoc)
import HPack.Monads
import HPack.JSON

data ExtractErr
    = ParseError { fileName :: FilePath, parseError :: String }
    | IfaceLookupError Pkg
    | ModuleLookupError Pkg ModulePath
    | SymbolLookupError Name [Symbol]

type ExtractM = HPackT ExtractErr () (IfaceRepoM IfaceM)

runExtractM :: ExtractM a -> IO (Either ExtractErr a)
runExtractM = undefined

liftIfaceRepo :: IfaceRepoM IfaceM a -> ExtractM a
liftIfaceRepo = lift

liftIface :: IfaceM a -> ExtractM a
liftIface = lift . lift

-- | Extract a package interface from
extract :: PkgDB -> PkgId -> CabalPkg -> ExtractM PkgInterface
extract pkgDB pkgId cabalPkg = do
    let modNames = exposedMods cabalPkg
    let Just (Pkg name version) = lookupPkg pkgDB pkgId
    modIfaces <- mapM (getModuleInterface pkgDB pkgId) modNames
    return $ PkgInterface name version (M.fromList (zip modNames modIfaces))

-- | Get an interface for the given module
getModuleInterface :: PkgDB -> PkgId -> ModulePath -> ExtractM ModInterface
getModuleInterface pkgDB pkgId modName = do
    let buildDir = getBuildDir pkgDB pkgId
    let dirWithCompiledFiles = buildDir </> "dist" </> "build"
    modIface <- liftIface $
        compileAndLoadIface dirWithCompiledFiles (show modName)
    let provided = providedSymbols modIface
    required <- requiredSymbols pkgDB pkgId modName modIface
    return (ModInterface provided required)

-- | Get all the exported symbols from the ModIface
providedSymbols :: ModIface -> [Symbol]
providedSymbols ModIface{..} =
    catMaybes [ extractExport decl mi_exports
                  | decl <- map snd mi_decls ]
    where
        extractExport :: IfaceDecl -> [AvailInfo] -> Maybe Symbol
        extractExport ifaceDecl (availInfo:availInfos)
            | Avail name <- availInfo
            , GHC.nameOccName name == ifName ifaceDecl
                = extractDecl ifaceDecl
            | AvailTC name constructors <- availInfo
            , GHC.nameOccName name == ifName ifaceDecl
                = do symbol <- extractDecl ifaceDecl
                     return $ sliceDecl symbol $ map extractName constructors
            | otherwise
                = extractExport ifaceDecl availInfos
        extractExport ifaceDecl []
                = Nothing

        -- | Slice any unexported entries from the Symbol
        sliceDecl :: Symbol -> [Name] -> Symbol
        sliceDecl symbol constructors
            -- TODO: implement
            = symbol

-- | Determine the requirements the package has of its dependencies
requiredSymbols :: PkgDB -> PkgId -> ModulePath -> ModIface
                -> ExtractM [RequiredSymbol]
requiredSymbols pkgDB pkgId modulePath modIface = do
        ModuleRequirements requiredSymbolNames <- parseSymbolFile
        mapM resolveSymbol requiredSymbolNames
    where
        -- | Parse the external symbols file for the module
        parseSymbolFile :: ExtractM ModuleRequirements
        parseSymbolFile = do
            let buildDir    = getBuildDir pkgDB pkgId
            let modFileName = buildDir </> "ExternalSymbols"
                                       </> show modulePath ++ ".json"
            contents <- liftIO $ BS.readFile modFileName
            tryEither $ mapLeft (ParseError modFileName) $
                parseSymbolsForModule contents

        -- | Lookup the required symbol name in the interface of the exporting
        -- package
        resolveSymbol :: RequiredSymbolName -> ExtractM RequiredSymbol
        resolveSymbol (RequiredSymbolName pkgOrigin modName requiredSymName)
            | BuiltinPkg pkgName <- pkgOrigin
                = undefined
            | ExternalPkg pkg <- pkgOrigin = do
                PkgInterface{..} <- lookupPkgIface pkg
                ModInterface{provides = ps} <-
                    tryMaybe (ModuleLookupError pkg modName)
                             (M.lookup modName modules)
                let matchName symbol = symName symbol == requiredSymName
                let matches = filter matchName ps
                let origin  = Origin pkg modName
                case matches of
                    [symbol] -> return (RequiredSymbol origin symbol)
                    symbols  -> throw (SymbolLookupError requiredSymName symbols)

        -- | Look up the interface for the given package
        lookupPkgIface :: Pkg -> ExtractM PkgInterface
        lookupPkgIface pkg
            = tryMaybe (IfaceLookupError pkg) =<<
                liftIfaceRepo (getPkgIface pkg)


extractDecl :: IfaceDecl -> Maybe Symbol
extractDecl IfaceId{..}
    | name     <- extractOccName ifName
    , typ      <- extractType ifType
    = Just (Fun name typ)
extractDecl IfaceData{..}
    | name     <- extractOccName ifName
    -- , kind     <- extractKind ifKind
        -- ^ this field does not exist in ghc 7.10
    , tyvars   <- map extractTypeVar ifTyVars
    , datacons <- extractDataCons ifCons
    -- NOTE: record fields are already exported as functions (IfaceId)
    = Just (DataType name tyvars datacons)
extractDecl IfaceSynonym{..}
    | name     <- extractOccName ifName
    , kind     <- extractKind ifSynKind
    , tyvars   <- map extractTypeVar ifTyVars
    , rhs      <- extractType ifSynRhs
    = Just (TypeSynonym name kind tyvars rhs)
extractDecl IfaceClass{..}
    | name     <- extractOccName ifName
    , sigs     <- S.fromList (map extractMethodSignature ifSigs)
    = Just (ClassDef name sigs)
extractDecl IfaceFamily{..}
    = Nothing -- TODO
extractDecl IfaceAxiom{..}
    = Nothing -- TODO
extractDecl IfacePatSyn{..}
    = Nothing -- TODO

extractType :: IfaceType -> Type
extractType (IfaceTyVar lclName)
    = TVarRef (extractLclName lclName)
extractType (IfaceLitTy (IfaceNumTyLit x))
    = TNum x
extractType (IfaceLitTy (IfaceStrTyLit x))
    = TStr (extractStr x)
extractType (IfaceAppTy t1 t2)
    = TApp (extractType t1) (extractType t2)
extractType (IfaceFunTy t1 t2)
    = TFun (extractType t1) (extractType t2)
extractType (IfaceDFunTy t1 t2)
    = TDataFun (extractType t1) (extractType t2)
#if __GLASGOW_HASKELL__ > 710
extractType (IfaceForAllTy (IfaceTv binder visibility) t)
    = TForAll (extractTypeVar binder)
#else
extractType (IfaceForAllTy binder t)
    = TForAll (extractTypeVar binder)
#endif
extractType (IfaceTyConApp tyCon args)
    | tyConName <- extractExtName (ifaceTyConName tyCon)
    , tyConArgs <- extractArgs args
    = TTyConApp tyConName tyConArgs


extractOccName :: OccName -> String
extractOccName = occNameString

extractLclName :: IfLclName -> Name
extractLclName = extractStr

extractExtName :: IfExtName -> Name
extractExtName = extractName

extractName :: GHC.Name -> Name
extractName = extractOccName . GHC.nameOccName

extractTypeVar :: IfaceTvBndr -> TypeVar
extractTypeVar (lclName, kind)
    = TypeVar (extractLclName lclName) (extractKind kind)

extractArgs :: IfaceTcArgs -> [(Type, Visibility)]
extractArgs ITC_Nil           = []
extractArgs (ITC_Type t args) = (extractType t, Visible) : extractArgs args
extractArgs (ITC_Kind t args) = (extractType t, Visible) : extractArgs args

extractStr :: FastString -> String
extractStr = unpackFS

-- This visibility stuff was introduced after GHC 7.10
-- extractVisibility :: VisibilityFlag -> Visibility
-- extractVisibility GHC.Visible   = Visible
-- extractVisibility GHC.Specified = Specified
-- extractVisibility GHC.Invisible = Invisible

extractKind :: IfaceKind -> Kind
extractKind = Kind . extractType

extractDataCons :: IfaceConDecls -> [DataCon]
extractDataCons (IfAbstractTyCon _) = []
extractDataCons IfDataFamTyCon      = []
extractDataCons (IfDataTyCon decls) = map extractDataCon decls
extractDataCons (IfNewTyCon decl)   = [extractDataCon decl]

extractDataCon :: IfaceConDecl -> DataCon
extractDataCon IfCon{..}
    = DataCon
        { dataConName    = extractOccName ifConOcc
        , dataConArgs    = map extractType ifConArgTys
        , existentials   = map extractTypeVar ifConExTvs
        , typeEqualities = [(extractLclName v, extractType t)
                                | (v,t) <- ifConEqSpec ]
        , context        = map extractType ifConCtxt
        }

-- | TODO: IfaceClassOp changed in GHC 7.10 or 8.1
extractMethodSignature :: IfaceClassOp -> Symbol
extractMethodSignature (IfaceClassOp name maybeImpl typ)
    = Fun (extractOccName name) (extractType typ)
