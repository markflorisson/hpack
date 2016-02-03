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

import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics

import HPack.Cabal (CabalPkg)
import HPack.Source (Pkg, ModulePath, Version)
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
    = RequiredSymbol Origin Symbol
    deriving (Eq, Ord, Show, Generic)

data Symbol
    = DataType
        { name      :: Name
        , typevars  :: [TypeVar]
        , datacons  :: [DataCon]
        }
        -- ^ abstract, newtype or data type definition.
    | TypeSynonym
        { name      :: Name
        , kind      :: Kind
        , typevars  :: [TypeVar]
        , typ       :: Type
        }
        -- ^ synonym of some other symbol
    | Fun
        { name      :: Name
        , typ       :: Type
        }
        -- ^ symbol binding (function or constant)
    | ClassDef
        { name      :: Name
        , bindings  :: S.Set Symbol
        }
        -- ^ type class declaration
    | ClassInst
        { name      :: Name
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


-- instance Generic IfaceType
-- instance Generic IfaceKind
-- instance Generic IfLclName
-- instance Generic IfaceTyLit
-- instance Generic IfaceTvBndr
-- instance Generic IfaceForAllBndr
-- instance Generic IfaceTyCon
-- instance Generic IfaceTcArgs
-- instance Generic FastString
-- instance Generic Name
-- instance Generic IfExtName
-- instance Generic IfaceCoercion
-- instance Generic IfaceTyConInfo
-- instance ToJSON FastString
-- instance ToJSON IfaceTcArgs
-- instance ToJSON IfaceType



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


type Mismatch = () -- TODO

-- | Check whether the requirements of the first package interface
-- are provided by the second package interface
match :: PkgInterface -> PkgInterface -> Either Mismatch ()
match iface1 iface2 = undefined

(<:) :: Type -> Type -> Bool
t1 <: t2 = undefined
