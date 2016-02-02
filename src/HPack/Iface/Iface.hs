{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module HPack.Iface.Iface
( PkgInterface(..)
, ModInterface(..)
, Symbol(..)
, DataCon(..)
, TypeVar(..)
, Kind(..)
, Origin(..)
, Type(..)
, Name(..)
, match
) where

import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics

import HPack.Cabal (CabalPkg)
import HPack.Source (Pkg, ModulePath)
import HPack.System.PkgDB (PkgDB, PkgId)
import HPack.Monads
import HPack.JSON

type Name = String
type Type = String

data PkgInterface
    = PkgInterface
        { pkgName :: String
        , pkgVersion :: String
        , modules :: M.Map ModulePath ModInterface
        }
        deriving (Eq, Ord, Show, Generic)

data ModInterface
    = ModInterface
        { provides :: [Symbol]
        , requires :: [Symbol]
        }
        deriving (Eq, Ord, Show, Generic)

data Symbol
    = DataType
        { name      :: Name
        , origin    :: Origin
        , typevars  :: [TypeVar]
        , datacons  :: [DataCon]
        }
        -- ^ abstract, newtype or data type definition.
    | TypeSynonym
        { name      :: Name
        , origin    :: Origin
        , kind      :: Kind
        , typevars  :: [TypeVar]
        , typ       :: Type
        }
        -- ^ synonym of some other symbol
    | Fun
        { name      :: Name
        , origin    :: Origin
        , typ       :: Type
        }
        -- ^ symbol binding (function or constant)
    | ClassDef
        { name      :: Name
        , origin    :: Origin
        , bindings  :: S.Set Symbol
        }
        -- ^ type class declaration
    | ClassInst
        { name      :: Name
        , origin    :: Origin
        }
        -- ^ type class instance definition
    deriving (Eq, Ord, Show, Generic)

data DataCon
    = DataCon Name Type
    deriving (Eq, Ord, Show, Generic)

data TypeVar = TypeVar Kind
    deriving (Eq, Ord, Show, Generic)

data Kind = Star | (|->) Kind Kind
    deriving (Eq, Ord, Show, Generic)

data Origin = Origin
    { exportingPkg :: Pkg           -- ^ the package that exports the symbol
    , exportingMod :: ModulePath    -- ^ path to exporting module
    }
    deriving (Eq, Ord, Show, Generic)


instance ToJSON PkgInterface
instance ToJSON ModInterface
instance ToJSON Symbol
instance ToJSON DataCon
instance ToJSON TypeVar
instance ToJSON Kind
instance ToJSON Origin

instance FromJSON PkgInterface
instance FromJSON ModInterface
instance FromJSON Symbol
instance FromJSON DataCon
instance FromJSON TypeVar
instance FromJSON Kind
instance FromJSON Origin

type Mismatch = () -- TODO

-- | Check whether the requirements of the first package interface
-- are provided by the second package interface
match :: PkgInterface -> PkgInterface -> Either Mismatch ()
match iface1 iface2 = undefined

(<:) :: Type -> Type -> Bool
t1 <: t2 = undefined
