{- | Load a Cabal file from the program argument.

Usage:

    loadCabal

-}
module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Version (Version(..))

import HPack.Package (PkgInfo(..), printPkgInfo)
import HPack.Cabal (loadCabalFromFile)

ghcVersion :: Version
ghcVersion = Version [7, 10, 1] []

main :: IO ()
main = do
    args <- getArgs
    case args of
        [arg] -> loadCabalFromFile ghcVersion arg
                 >>= printPkgInfo
        _     -> putStrLn "Expected a .cabal filename argument"
                 >> exitFailure
