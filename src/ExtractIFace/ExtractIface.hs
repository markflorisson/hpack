module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import HPack.Iface
    (ModName(..), runIfaceM, compileAndLoadIface, showModIface, extract)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [modName] -> loadIface modName
        _         -> failure "Expected a module name as argument"

loadIface :: String -> IO ()
loadIface modName = do
    msg <- runIfaceM $ do
        modIface <- compileAndLoadIface "." modName
        -- iface <- lift $ extract modIface
        showModIface modIface
    putStrLn msg

exit exitCode s = putStrLn s >> exitWith exitCode
success = exit ExitSuccess
failure = exit (ExitFailure 1)
