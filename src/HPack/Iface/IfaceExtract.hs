{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

{- |

Extract interfaces for compiled packages.
-}

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
import qualified Data.List as L
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

type ExtractM m = HPackT ExtractErr () (IfaceRepoM m)

runExtractM :: Monad m => ExtractM m a -> IfaceRepoM m (Either ExtractErr a)
runExtractM m = runHPackT m ()

liftIfaceRepo :: Monad m => IfaceRepoM m a -> ExtractM m a
liftIfaceRepo = lift

liftIface :: MonadIO m => IfaceM a -> ExtractM m a
liftIface = liftIO . runIfaceM

-- | Extract a package interface from
extract :: MonadIO m => PkgDB -> PkgId -> CabalPkg
        -> ExtractM m PkgInterface
extract pkgDB pkgId cabalPkg = do
    let modNames = exposedMods cabalPkg
    let Just (Pkg name version) = lookupPkg pkgDB pkgId

    symbols <- mapM (getSymbols pkgDB pkgId) modNames

    let (provided, required) = unzip symbols
    providedModules <- return $ M.fromList
        [ (modName, ModInterface modName exports)
        | (modName, exports) <- zip modNames provided ]
    let deps = groupSymbols (concat required)
    return $ PkgInterface name version providedModules deps

-- | Get an interface for the given module
getSymbols :: MonadIO m => PkgDB -> PkgId -> ModulePath
           -> ExtractM m ([Symbol], [(PkgName, ModulePath, Symbol)])
getSymbols pkgDB pkgId modName = do
    let buildDir = getBuildDir pkgDB pkgId
    let dirWithCompiledFiles = buildDir </> "dist" </> "build"
    modIface <- liftIface $
        compileAndLoadIface dirWithCompiledFiles (show modName)
    required <- requiredSymbols pkgDB pkgId modName modIface
    return (providedSymbols modIface, required)

-- | Group exported symbols by package and module
groupSymbols :: [(PkgName, ModulePath, Symbol)] -> [(PkgName, Modules)]
groupSymbols = undefined -- map makeModules . groupBy' (\x y z -> x)
    where
        first (x, y, z)  = x
        second (x, y, z) = y
        third (x, y, z)  = z

        groupBy' :: (Ord d, Eq d) => ((a, b, c) -> d) -> [(a, b, c)] -> [[(a, b, c)]]
        groupBy' proj = L.groupBy (\x y -> proj x == proj y)
                      . L.sortBy (\x y -> compare (proj x) (proj y))

        -- | Given a list of symbols for the *same* package name, create a
        -- mapping from module paths to module interfaces
        makePkg :: [(PkgName, ModulePath, Symbol)] -> (PkgName, Modules)
        makePkg xs
            | pkgName <- first (head xs)
            , modules <- groupBy' second xs
            = (pkgName, foldMap makeModules modules)

        -- | Given a list of symbols for the *same* module name, create a
        -- singleton dictionary mapping the module name to the module interface
        -- (to be folded with mappend)
        makeModules :: [(PkgName, ModulePath, Symbol)] -> Modules
        makeModules xs
            | modName <- second (head xs)
            , symbols <- map third xs
            = M.singleton modName (ModInterface modName symbols)

-- | Get all the exported symbols from the ModIface
providedSymbols :: ModIface -> [Symbol]
providedSymbols ModIface{..} =
    catMaybes [ extractExport decl mi_exports
                  | decl <- map snd mi_decls ]
    where
        -- | Search for the interface declaration `ifaceDecl` in the
        -- list of exported symbols. If present, convert the declaration
        -- to a Symbol. If not present, return Nothing.
        extractExport :: IfaceDecl -> [AvailInfo] -> Maybe Symbol
        extractExport ifaceDecl (availInfo:availInfos)
            | Avail name <- availInfo
            , GHC.nameOccName name == ifName ifaceDecl
                -- regular name, just extract a normal symbol name
                = extractDecl ifaceDecl
            | AvailTC name constructors <- availInfo
            , GHC.nameOccName name == ifName ifaceDecl
                -- type name, extract a symbol and slice away any unexported
                -- data constructors
                = do symbol <- extractDecl ifaceDecl
                     return $ sliceDecl symbol $ map extractName constructors
            | otherwise
                -- keep searching
                = extractExport ifaceDecl availInfos
        extractExport ifaceDecl []
                = Nothing

        -- | Slice any unexported entries from the Symbol
        sliceDecl :: Symbol -> [Name] -> Symbol
        sliceDecl symbol constructors
            -- TODO: implement
            = symbol

-- | Determine the requirements the package has of its dependencies
requiredSymbols :: MonadIO m => PkgDB -> PkgId -> ModulePath -> ModIface
                -> ExtractM m [(PkgName, ModulePath, Symbol)]
requiredSymbols pkgDB pkgId modulePath modIface = do
        ModuleRequirements requiredSymbolNames <- parseSymbolFile
        catMaybes <$> mapM resolveSymbol requiredSymbolNames
    where
        -- | Parse the external symbols file for the module
        parseSymbolFile :: MonadIO m => ExtractM m ModuleRequirements
        parseSymbolFile = do
            let buildDir    = getBuildDir pkgDB pkgId
            let modFileName = buildDir </> "ExternalSymbols"
                                       </> show modulePath ++ ".json"
            contents <- liftIO $ BS.readFile modFileName
            tryEither $ mapLeft (ParseError modFileName) $
                parseSymbolsForModule contents

        -- | Lookup the required symbol name in the interface of the exporting
        -- package
        resolveSymbol :: MonadIO m
                      => RequiredSymbolName
                      -> ExtractM m (Maybe (PkgName, ModulePath, Symbol))
        resolveSymbol (RequiredSymbolName pkgOrigin modName requiredSymName)
            | BuiltinPkg pkgName <- pkgOrigin
                -- TODO: check that this package is in our "blessed" set of
                -- built-in packages
                = return Nothing
            | ExternalPkg pkg@(Pkg pkgName pkgVersion) <- pkgOrigin = do
                PkgInterface{providedModules = modules} <- lookupPkgIface pkg
                ModInterface{moduleExports = exports} <-
                    tryMaybe (ModuleLookupError pkg modName)
                             (M.lookup modName modules)
                let matchName symbol = symName symbol == requiredSymName
                let matches = filter matchName exports
                case matches of
                    [symbol] -> return $ Just (pkgName, modName, symbol)
                    symbols  -> throw (SymbolLookupError requiredSymName symbols)

        -- | Look up the interface for the given package
        lookupPkgIface :: MonadIO m => Pkg -> ExtractM m PkgInterface
        lookupPkgIface pkg
            = tryMaybe (IfaceLookupError pkg) =<<
                liftIfaceRepo (getPkgIface pkg)

-- | Extract a symbol from a symbol declaration in a .hi file
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

-- extractExternalOrigin :: GHC.Name -> Origin
-- extractExternalOrigin ghcName
--     = getMod (n_sort ghcName)
--     where
--         getMod (External m)    = m
--         getMod (WiredIn m _ _) = m

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
