module Main where

import HPack.Iface (runIfaceM, compileAndLoadIface, showModIface)

main :: IO ()
main = do
    eitherModIface <- runIfaceM $ compileAndLoadIface "Mod" >>= showModIface
    case eitherModIface of
        Left err       -> print err
        Right ifaceStr -> putStrLn ifaceStr
    return ()
