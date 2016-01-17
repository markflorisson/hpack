{-# LANGUAGE RecordWildCards #-}

module HPack.Infer.Infer
(PkgInterface(..), ModInterface(..), Origin, Signature, extract)
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
-- compiler/iface/IfaceSyn
import IfaceSyn (IfaceDecl(..))
import GHC.Fingerprint (Fingerprint)

import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)

import HPack.Source (ModulePath)
import HPack.System (PkgId)
import HPack.Ghc (SDoc, ghcShow, ghcShowSDoc)

type Signature = String

data PkgInterface
    = PkgInterface
        { name :: String
        , version :: String
        , modules :: M.Map ModulePath ModInterface
        }

data ModInterface
    = ModInterface
        { provides :: [Symbol]
        , requires :: [Symbol]
        }
        deriving (Eq, Show)

data Symbol
    = TyCon    String Origin Signature
    | DataCon  String Origin Signature
    | Fun      String Origin Signature
    | ClassDef String Origin
    | ClassInst String Origin
    deriving (Eq, Show)

data Origin = Origin
    { immediateDepence :: PkgId -- ^ the package that exports the symbol
    , source :: PkgId           -- ^ the original module that defined the
                                --   symbol (in the case of a type constructor)
    }
    deriving (Eq, Show)

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
