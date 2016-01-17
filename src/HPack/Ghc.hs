module HPack.Ghc
( DynFlags, initDynFlags, getDynFlags
, SDoc, ghcShow, ghcShowSDoc
) where

import GHC (Ghc, getSessionDynFlags, setSessionDynFlags)
import Outputable (Outputable(..), showSDoc, ppr, SDoc)
import DynFlags (DynFlags, defaultFatalMessager, defaultFlushOut)

import Control.Monad (void)

---------------------------------------------------------

initDynFlags :: Ghc ()
initDynFlags = void $ setSessionDynFlags =<< getDynFlags

getDynFlags :: Ghc DynFlags
getDynFlags = getSessionDynFlags

---------------------------------------------------------

ghcShow :: Outputable a => a -> Ghc String
ghcShow x = ghcShowSDoc (ppr x)

ghcShowSDoc :: SDoc -> Ghc String
ghcShowSDoc sdoc = do
    dynflags <- getDynFlags
    return $ showSDoc dynflags sdoc
