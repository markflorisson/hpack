{-# LANGUAGE DeriveGeneric #-}

module HPack.Source.Package
( Pkg(..)
, Version(..)
, ModulePath(..)
, showVersion
) where

import Data.List (intercalate)
import Data.Version (Version(..))

import GHC.Generics

-- | A package name and version pair
data Pkg = Pkg { pkgName :: String, pkgVersion :: Version }
    deriving (Eq, Ord, Generic)

-- | Path to a module within some package
data ModulePath = ModulePath [String]
    deriving (Eq, Ord, Generic)

---------------------------------------------------------

instance Show Pkg where
    show (Pkg name version) = name ++ "-" ++ showVersion version

showVersion :: Version -> String
showVersion (Version versionInfo tags)
    = intercalate "." (map show versionInfo ++ tags)
