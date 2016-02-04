{-# LANGUAGE DeriveGeneric #-}

module HPack.Source.Package
( Pkg(..)
, V.Version(..)
, ModulePath(..)
, mkModulePath
, showVersion
, parseVersion
) where

import Data.List (intercalate)
import Text.ParserCombinators.ReadP (readP_to_S)
import qualified Data.Version as V
import qualified Data.Text as T
import Data.String.Utils (split)

import GHC.Generics

import HPack.JSON

-- | A package name and version pair
data Pkg = Pkg { pkgName :: String, pkgVersion :: V.Version }
    deriving (Eq, Ord, Generic)

-- | Path to a module within some package
data ModulePath = ModulePath [String]
    deriving (Eq, Ord, Generic)

mkModulePath :: String -> ModulePath
mkModulePath = ModulePath . split "."

---------------------------------------------------------

instance Show Pkg where
    show (Pkg name version) = name ++ "-" ++ showVersion version

instance Show ModulePath where
    show (ModulePath xs) = intercalate "." xs

showVersion :: V.Version -> String
showVersion (V.Version versionInfo tags)
    = intercalate "." (map show versionInfo ++ tags)

parseVersion :: String -> Maybe V.Version
parseVersion versionString =
    case readP_to_S V.parseVersion versionString of
        (version, _):xs -> Just version
        _            -> Nothing

---------------------------------------------------------

instance ToJSON Pkg
instance FromJSON Pkg

instance ToJSON ModulePath
instance FromJSON ModulePath

instance ToJSON V.Version where
    toJSON = toJSON . showVersion

instance FromJSON V.Version where
    parseJSON (String text)
        | Just version <- parseVersion (T.unpack text)
            = return version
        | otherwise
            = fail $ "Unable to parse version string: '" ++ show text ++ "'"
    parseJSON val = typeMismatch "Version" val
