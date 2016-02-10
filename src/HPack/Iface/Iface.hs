{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

{- |

This module provides the interface data type. Suppose we have a package
P of version 1 which provides the following module:

    ------------- Definition of package P-v1 -------------
    module P.Foo where
    import Data.ByteString (empty, append)
    foo s =
        if s /= empty
          then append s s
          else empty
    ------------- Definition of package P-v1 -------------

In this definition we use the 'empty' and 'append' functions from
the Data.ByteString module from the ByteString package. We further
use the (in)equality function from the Eq type class. The interface
for a package definition such as above would look as follows:

    ------------- Interface for package P-v1 -------------
    pkg P-v1 where
        provides module P.Foo where
            foo :: [L1]Data.ByteString.ByteString
                -> [L1]Data.ByteString.ByteString

        requires pkg ByteString as L1:
            module Data.ByteString:
                data ByteString :: *

                instance Prelude.Eq [L1]Data.ByteString.ByteString

                empty  :: [L1]Data.ByteString.ByteString
                append :: [L1]Data.ByteString.ByteString
                       -> [L1]Data.ByteString.ByteString
                       -> [L1]Data.ByteString.ByteString

        requires pkg base as L2:
            module Prelude:
                class Eq a where
                    (==) :: a -> a -> Bool
                    (/=) :: a -> a -> Bool
                    ...
    ------------- Interface for package P-v1 -------------

Here we have labelled the abstract data type ByteString by its package
label. This is used to that we can represent disjoint requirements,
so that we can duplicate packages. For example, we may have an interface
as follows:

    ------------- Interface for package Q-v1 -------------
    pkg Q-v1 where
        provides module Q.QMod where
            f1 :: [L1]Data.ByteString.ByteString -> Int
            f2 :: [L2]Data.ByteString.Lazy.ByteString -> Int

        requires pkg ByteString as L1:
            module Data.ByteString:
                data ByteString :: *
                ...

        required pkg ByteString as L2:
            module Data.ByteString.Lazy:
                data ByteString :: *
                ...

    ------------- Interface for package Q-v1 -------------

Note that when we require some symbol from a dependee with a given signature,
we also require the package and module that defines any of the abstract
data types mentioned in the signature. For example, a package R-v1
may depend on Q.f1:

    ------------- Definition of package R-v1 -------------
    module R.RMod where
    import Q.QMod (f1)
    g = f1
    ------------- Definition of package R-v1 -------------

    ------------- Interface for package R-v1 -------------
    pkg R-v1 where
        provides module R.RMod where
            g :: [L5]Data.ByteString.ByteString -> Int

        requires pkg ByteString (L5):
            module Data.ByteString:
                data ByteString :: *
    ------------- Interface for package R-v1 -------------

Of course the labels are not globally unique, they are only unique within
the package interface definition.

This approach exposes sharing constraints locally available by merging package
requirements under a single label whenever we have a non-empty intersection
of exposed abstract data types.
-}

module HPack.Iface.Iface
( PkgInterface(..)
, ModInterface(..)
, Modules(..)
, Symbol(..)
, DataCon(..)
, TypeVar(..)
, Kind(..)
, Type(..)
, Visibility(..)
, PkgName(..)
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

type PkgName      = String
type Name         = String
type ModuleName   = ModulePath

type Modules      = M.Map ModuleName ModInterface

data PkgInterface
    = PkgInterface
        { pkgName               :: PkgName
        , pkgVersion            :: Version
        , providedModules       :: Modules
            -- ^ Interfaces for the modules provided by this package
        , dependencies          :: [(PkgName, Modules)]
            -- ^ package dependency requirements
            -- This list may have duplicate entries for the same package name
            -- whenever we have registered multiple requirements on the same
            -- package without requiring an equality on package versions
        }
        deriving (Eq, Ord, Show, Generic)

data ModInterface
    = ModInterface
        { moduleName    :: ModuleName
        , moduleExports :: [Symbol]
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


instance ToJSON PkgInterface
instance ToJSON ModInterface
instance ToJSON Symbol
-- instance ToJSON RequiredSymbol
instance ToJSON DataCon
instance ToJSON Type
instance ToJSON TypeVar
instance ToJSON Visibility
instance ToJSON Kind

instance FromJSON PkgInterface
instance FromJSON ModInterface
instance FromJSON Symbol
-- instance FromJSON RequiredSymbol
instance FromJSON DataCon
instance FromJSON Type
instance FromJSON TypeVar
instance FromJSON Visibility
instance FromJSON Kind


data Mismatch
    = NoSuchModule PkgInterface ModulePath
    | NoSuchSymbol PkgInterface ModulePath Name
    | SymbolTypeMismatch PkgInterface ModulePath Symbol Symbol
    deriving (Eq, Show)

-- | Check whether the requirements of the first package interface
-- are provided by the second package interface
match :: Modules -> PkgInterface -> Either Mismatch ()
match requiredModules dependee
    | providedModules <- providedModules dependee
    = matchModules requiredModules providedModules
    where
        matchModules :: Modules -> Modules -> Either Mismatch ()
        matchModules requiredModules providedModules =
            forM_ (M.assocs requiredModules) $ \(modName, requiredModIface) ->
                case M.lookup modName providedModules of
                    Just providedModIface ->
                        matchModuleIface requiredModIface providedModIface
                    Nothing ->
                        Left $ NoSuchModule dependee modName

        matchModuleIface :: ModInterface -> ModInterface -> Either Mismatch ()
        matchModuleIface requiredModIface providedModIface
            = mapM_ (matchSymbol providedModIface) (moduleExports requiredModIface)

        matchSymbol :: ModInterface -> Symbol -> Either Mismatch ()
        matchSymbol ModInterface{..} requiredSym
            | [providedSym] <-
                [ providedSym | providedSym <- moduleExports
                              , symName providedSym == symName requiredSym ]
                = if providedSym <: requiredSym
                    then Right ()
                    else Left (SymbolTypeMismatch dependee moduleName
                                                  providedSym requiredSym)
            | otherwise
                = Left $ NoSuchSymbol dependee moduleName (symName requiredSym)

-- | Check whether the first symbol is a subtype of the second
--
-- TODO: an actual definition of subtyping
(<:) :: Symbol -> Symbol -> Bool
sym1 <: sym2 = sym1 == sym2
