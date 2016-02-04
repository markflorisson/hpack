{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module HPack.Iface.Iface
( PkgInterface(..)
, ModInterface(..)
, Symbol(..)
, RequiredSymbol(..)
, DataCon(..)
, TypeVar(..)
, Kind(..)
, Origin(..)
, Type(..)
, Visibility(..)
, Name(..)
, match
) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics

import HPack.Cabal (CabalPkg)
import HPack.Source (Pkg(..), ModulePath, Version)
import HPack.System.PkgDB (PkgDB, PkgId)
import HPack.Monads
import HPack.JSON

type Name = String

data PkgInterface
    = PkgInterface
        { pkgName :: String
        , pkgVersion :: Version
        , modules :: M.Map ModulePath ModInterface
        }
        deriving (Eq, Ord, Show, Generic)

data ModInterface
    = ModInterface
        { provides :: [Symbol]
        , requires :: [RequiredSymbol]
        }
        deriving (Eq, Ord, Show, Generic)

data RequiredSymbol
    = RequiredSymbol
        { symbolOrigin :: Origin
        , getSym       :: Symbol
        }
        deriving (Eq, Ord, Show, Generic)

data Symbol
    = DataType
        { symName   :: Name
        , typevars  :: [TypeVar]
        , datacons  :: [DataCon]
        }
        -- ^ abstract, newtype or data type definition.
    | TypeSynonym
        { symName   :: Name
        , kind      :: Kind
        , typevars  :: [TypeVar]
        , typ       :: Type
        }
        -- ^ synonym of some other symbol
    | Fun
        { symName   :: Name
        , typ       :: Type
        }
        -- ^ symbol binding (function or constant)
    | ClassDef
        { symName   :: Name
        , bindings  :: S.Set Symbol
        }
        -- ^ type class declaration
    | ClassInst
        { symName   :: Name
        }
        -- ^ type class instance definition
    deriving (Eq, Ord, Show, Generic)

data DataCon
    = DataCon
        { dataConName    :: Name
        , dataConArgs    :: [Type]
        , existentials   :: [TypeVar]
        , typeEqualities :: [(Name, Type)]
        , context        :: [Type]
        }
    deriving (Eq, Ord, Show, Generic)

data Type
    = TVarRef Name
    | TNum Integer
    | TStr String
    | TApp Type Type
    | TFun Type Type
    | TDataFun Type Type
    | TForAll TypeVar
    | TTyConApp Name [(Type, Visibility)]
    | TTuple [(Type, Visibility)]
        -- TODO: Is TupleSort relevant for the interface?
    deriving (Eq, Ord, Show, Generic)

data Visibility = Visible | Specified | Invisible
    deriving (Eq, Ord, Show, Generic)

data TypeVar = TypeVar Name Kind
    deriving (Eq, Ord, Show, Generic)

data Kind
    -- = Star | (|->) Kind Kind
    = Kind Type -- IfaceKind seems to be an alias of IfaceType in GHC
    deriving (Eq, Ord, Show, Generic)

data Origin = Origin
    { exportingPkg :: Pkg           -- ^ the package that exports the symbol
    , exportingMod :: ModulePath    -- ^ path to exporting module
    }
    deriving (Eq, Ord, Show, Generic)

instance ToJSON PkgInterface
instance ToJSON ModInterface
instance ToJSON Symbol
instance ToJSON RequiredSymbol
instance ToJSON DataCon
instance ToJSON Type
instance ToJSON TypeVar
instance ToJSON Visibility
instance ToJSON Kind
instance ToJSON Origin

instance FromJSON PkgInterface
instance FromJSON ModInterface
instance FromJSON Symbol
instance FromJSON RequiredSymbol
instance FromJSON DataCon
instance FromJSON Type
instance FromJSON TypeVar
instance FromJSON Visibility
instance FromJSON Kind
instance FromJSON Origin


data Mismatch
    = NoSuchModule PkgInterface ModulePath
    | NoSuchSymbol PkgInterface ModulePath Name
    | SymbolTypeMismatch PkgInterface ModulePath Symbol Symbol
    deriving (Eq, Show)

-- | Check whether the requirements of the first package interface
-- are provided by the second package interface
match :: PkgInterface -> PkgInterface -> Either Mismatch ()
match dependent dependee
    | requiredSymbols       <- requirementsBySourceModule dependent
    , symbolToSourceModule  <- M.fromList requiredSymbols
    , requirementsByDestPkg <- M.fromList $ sortByPackage $ map fst requiredSymbols
    , Just required         <- M.lookup (getPkgFromIface dependee) requirementsByDestPkg
        = mapM_ (getDefinition dependee) required

-- | Lookup the a required symbol in a package that is supposed to define it
-- If the package does not define an appropriate symbol with a subtype of the
-- required type, raise an exception. Otherwise, return the provided symbol.
getDefinition :: PkgInterface -> RequiredSymbol -> Either Mismatch Symbol
getDefinition pkgIface@PkgInterface{..} requiredSym
    | Just modIface <- M.lookup modName modules
        = lookupSymbol modIface
    | otherwise
        = Left (NoSuchModule pkgIface modName)
    where
        modName :: ModulePath
        modName = getMod requiredSym

        name :: Name
        name = symName (getSym requiredSym)

        lookupSymbol :: ModInterface -> Either Mismatch Symbol
        lookupSymbol ModInterface{..}
            | [sym] <- [ sym | sym <- provides, symName sym == name ]
                = if sym <: getSym requiredSym
                    then Right sym
                    else Left $
                        SymbolTypeMismatch pkgIface modName sym (getSym requiredSym)
            | otherwise
                = Left (NoSuchSymbol pkgIface modName name)

requirementsBySourceModule :: PkgInterface -> [(RequiredSymbol, ModulePath)]
requirementsBySourceModule pkgInterface
    = [ (requiredSymbol, modName)
            | (modName, modIface) <- M.assocs (modules pkgInterface)
            , requiredSymbol <- requires modIface ]

sortByPackage :: [RequiredSymbol] -> [(Pkg, [RequiredSymbol])]
sortByPackage requiredSymbols
    | sorted  <- L.sortBy byPkg requiredSymbols
    , grouped <- L.groupBy byPkg' sorted
        = [ (getPkg (head group), group) | group <- grouped ]
    where
        byPkg sym1 sym2  = compare (getPkg sym1) (getPkg sym2)
        byPkg' sym1 sym2 = getPkg sym1 == getPkg sym2

getPkgFromIface :: PkgInterface -> Pkg
getPkgFromIface PkgInterface{..} = Pkg pkgName pkgVersion

getPkg :: RequiredSymbol -> Pkg
getPkg = exportingPkg . symbolOrigin

getMod :: RequiredSymbol -> ModulePath
getMod = exportingMod . symbolOrigin

-- | Check whether the first symbol is a subtype of the second
--
-- TODO: an actual definition of subtyping
(<:) :: Symbol -> Symbol -> Bool
sym1 <: sym2 = sym1 == sym2
