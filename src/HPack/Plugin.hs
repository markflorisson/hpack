module HPack.GhcPlugin
where

-- compiler/main/* from the GHC source tree
import Plugins (Plugin(..), defaultPlugin)
-- compiler/typecheck/*
import TcPluginM (tcPluginIO)
import TcRnTypes
    (Ct, TcPlugin(..), TcPluginResult(..), TcPluginM)

-- ghc-tcplugins-extra
import GHC.TcPluginM.Extra ( evByFiat, tracePlugin, lookupModule, lookupName )

type TcPluginSolver = [Ct]    -- given
                   -> [Ct]    -- derived
                   -> [Ct]    -- wanted
                   -> TcPluginM TcPluginResult

data PluginState = PluginState

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = \opts -> Just hpackPlugin }

hpackPlugin :: TcPlugin
hpackPlugin = tracePlugin "HPack-plugin"
    TcPlugin { tcPluginInit  = initPlugin
             , tcPluginSolve = solver
             , tcPluginStop  = destroyPlugin
             }

initPlugin :: TcPluginM PluginState
initPlugin = return PluginState

solver :: PluginState -> TcPluginSolver
solver PluginState given derived wanted = do
    tcPluginIO $ putStrLn "hi!"
    let solved = undefined
    let unsolved = undefined
    return $ TcPluginOk solved unsolved

destroyPlugin :: PluginState -> TcPluginM ()
destroyPlugin PluginState = return ()
