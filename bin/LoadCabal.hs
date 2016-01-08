module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Version (Version(..))

import HPack.Cabal (PkgInfo(..), loadCabalFile, printPkgInfo)

ghcVersion :: Version
ghcVersion = Version [7, 10, 1] []

main :: IO ()
main = do
    args <- getArgs
    case args of
        [arg] -> loadCabalFile ghcVersion arg
                 >>= printPkgInfo
        _     -> putStrLn "Expected a .cabal filename argument"
                 >> exitFailure
