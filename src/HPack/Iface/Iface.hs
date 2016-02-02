{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module HPack.Iface.Iface
( PkgInterface(..)
, ModInterface(..)
, Symbol(..)
, Origin(..)
, Signature(..)
, Name(..)
, match
) where

import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)
import GHC.Generics

import HPack.Cabal (CabalPkg)
import HPack.Source (Pkg, ModulePath)
import HPack.System.PkgDB (PkgDB, PkgId)
import HPack.JSON

type Name       = String
type Signature  = String

data PkgInterface
    = PkgInterface
        { name :: String
        , version :: String
        , modules :: M.Map ModulePath ModInterface
        }
        deriving (Generic)

data ModInterface
    = ModInterface
        { provides :: [Symbol]
        , requires :: [Symbol]
        }
        deriving (Eq, Show, Generic)

data Symbol
    = TyCon     Name Origin Signature
    | DataCon   Name Origin Signature
    | Fun       Name Origin Signature
    | ClassDef  Name Origin
    | ClassInst Name Origin
    deriving (Eq, Show, Generic)

data Origin = Origin
    { exportingPkg :: PkgId         -- ^ the package that exports the symbol
    , exportingMod :: ModulePath    -- ^ path to exporting module
    }
    deriving (Eq, Show, Generic)


instance ToJSON PkgInterface
instance ToJSON ModInterface
instance ToJSON Symbol
instance ToJSON Origin

instance FromJSON PkgInterface
instance FromJSON ModInterface
instance FromJSON Symbol
instance FromJSON Origin

type Mismatch = () -- TODO

-- | Check whether the requirements of the first package interface
-- are provided by the second package interface
match :: PkgInterface -> PkgInterface -> Either Mismatch ()
match iface1 iface2 = undefined

(<:) :: Signature -> Signature -> Bool
t1 <: t2 = undefined
