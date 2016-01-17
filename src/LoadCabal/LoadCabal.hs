{- | Load a Cabal file from the program argument.

Usage:

    loadCabal myPkg.cabal

-}
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import HPack.Source (Version(..))
import HPack.Cabal (printCabalPkg, loadCabalFromFile)

ghcVersion :: Version
ghcVersion = Version [7, 10, 1] []

main :: IO ()
main = do
    args <- getArgs
    case args of
        [arg] -> loadCabalFromFile ghcVersion arg
                 >>= printCabalPkg
        _     -> putStrLn "Expected a .cabal filename argument"
                 >> exitFailure
