{-# LANGUAGE OverloadedStrings #-}

{-| This modules handles the parsing of .json files written out by our
modified GHC. Here is an example:

    {
       "definingPackage":"main",
       "exportedNames":[
          {
             "package":"ghc-prim",
             "module":"GHC.Classes",
             "name":"Eq"
          },
          {
             "package":"base",
             "module":"GHC.Base",
             "name":"id"
          },
          {
             "package":"bytestring-0.10.7.0",
             "module":"Data.ByteString",
             "name":"empty"
          },
          {
             "package":"bytestring-0.10.7.0",
             "module":"Data.ByteString",
             "name":"append"
          },
          {
             "package":"bytestring-0.10.7.0",
             "module":"Data.ByteString.Lazy.Internal",
             "name":"ByteString"
          }
       ]
    }
-}

module HPack.Iface.ExternalSymbols
( ModuleRequirements(..)
, RequiredSymbolName(..)
, PkgOrigin(..)
, parseSymbolsForModule
) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BS
import Data.String.Utils (split)

import HPack.Source ( Pkg(..), ModulePath(..), Version(..)
                    , mkModulePath, parseVersion)
import HPack.JSON

type Name = String

-- | Module containing a list of exported symbols
data ModuleRequirements
    = ModuleRequirements [RequiredSymbolName]
    deriving (Show)

data RequiredSymbolName
    = RequiredSymbolName PkgOrigin ModulePath Name
    deriving (Show)

data PkgOrigin
    = BuiltinPkg Name
    | ExternalPkg Pkg
    deriving (Show)

instance FromJSON ModuleRequirements where
    parseJSON (Object mod) = do
        symbols <- mod .: "exportedNames"
        ModuleRequirements <$> mapM parseJSON symbols
    parseJSON val
        = typeMismatch "Expected an ModuleRequirements (a JSON Object)" val

instance FromJSON RequiredSymbolName where
    parseJSON (Object sym) = do
        pkgOrigin <- parsePkgOrigin =<< sym .: "package"
        RequiredSymbolName <$> (parsePkgOrigin =<< sym .: "package")
                           <*> fmap mkModulePath (sym .: "module")
                           <*> sym .: "name"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON val
        = typeMismatch "Expected an RequiredSymbolName (a JSON Object)" val

parsePkgOrigin :: Value -> Parser PkgOrigin
parsePkgOrigin json@(String txt) = do
    let s = textToString txt
    case split "-" s of
        [pkgName, pkgVersion] ->
            case parseVersion pkgVersion of
                Just version
                    -> return $ ExternalPkg $ Pkg pkgName version
                Nothing
                    -> return $ BuiltinPkg s
        [pkgName] -> return $ BuiltinPkg pkgName
        _
            -> typeMismatch ("Expected a pkgName-pkgVersion, got " ++ s) json
parsePkg json =
    typeMismatch "Expected a package string name" json

parseSymbolsForModule :: BS.ByteString -> Either String ModuleRequirements
parseSymbolsForModule = eitherDecode
