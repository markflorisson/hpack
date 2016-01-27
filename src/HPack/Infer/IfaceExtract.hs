{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module HPack.Infer.IfaceExtract
(PkgInterface(..), ModInterface(..), Symbol(..), Origin(..), Signature(..)
, extractPkgInterface, extract
) where

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
-- compiler/iface/IfaceSyn
import IfaceSyn (IfaceDecl(..))
import GHC.Fingerprint (Fingerprint)
import GHC.Generics

import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)

import HPack.Cabal (CabalPkg)
import HPack.Source (Pkg, ModulePath)
import HPack.System (PkgDB, PkgId)
import HPack.Ghc (SDoc, ghcShow, ghcShowSDoc)

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
    { exportingPkg :: PkgId     -- ^ the package that exports the symbol
    }
    deriving (Eq, Show, Generic)

extractPkgInterface :: PkgDB -> PkgId -> Pkg -> IO PkgInterface
extractPkgInterface = undefined

extractModInterface :: PkgDB -> PkgId -> ModulePath -> IO ModInterface
extractModInterface pkgDB pkdId path =
    ModInterface <$> providedNames pkgDB path
                 <*> requiredNames pkgDB path

providedNames :: PkgDB -> ModulePath -> IO [Symbol]
providedNames pkgdb path = undefined

requiredNames :: PkgDB -> ModulePath -> IO [Symbol]
requiredNames pkgdb path = undefined

resolveSymbol :: Name -> Origin -> Symbol
resolveSymbol = undefined

(<:) :: Signature -> Signature -> Bool
t1 <: t2 = undefined

extract :: ModIface -> Ghc ModInterface
extract modIface = do
    liftIO $ putStrLn "Exported Symbols....................."
    exports <- mapM (ghcShow . availName) (mi_exports modIface)
    liftIO $ mapM_ print exports

    liftIO $ putStrLn "Exported..................."
    decls <- mapM ghcShow (mi_decls modIface)
    liftIO $ mapM_ print decls
    -- (fingerPrint, decl) |

    liftIO $ putStrLn "Exported list......................"
    syms <- mapM ghcShowSDoc (map declKind (mi_decls modIface))
    liftIO $ mapM_ print syms

    return $ ModInterface [] []

extractExport :: AvailInfo -> SDoc
extractExport export =
    ppr $ availName export

declKind :: (Fingerprint, IfaceDecl) -> SDoc
declKind (hash, decl)
    | IfaceId{..}   <- decl    = p "IfaceId"
    | IfaceData{..} <- decl    = p "IfaceData"
    | IfaceSynonym{..} <- decl = p "IfaceSynonym"
    | IfaceFamily{..}  <- decl = p "IfaceFamily"
    | IfaceClass{..} <- decl   = p "IfaceClass"
    | IfaceAxiom{..} <- decl   = p "IfaceAxiom"
    | IfacePatSyn{..} <- decl  = p "IfacePatSyn"
    where
        p :: String -> SDoc
        p ctor = ppr hash <+> text ctor <> text ":" <+> ppr (ifName decl)

---------------------------------------------------------
