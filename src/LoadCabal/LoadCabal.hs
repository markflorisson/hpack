{- | Load a Cabal file from the program argument.

Usage:

    loadCabal myPkg.cabal

-}
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import HPack.Source (Version(..))
import HPack.Cabal ( printCabalPkg, loadCabalFromFile
                   , parseCabalFile, writeCabalFile
                   )
import HPack.Config (Config, defaultConfig)

ghcVersion :: Version
ghcVersion = Version [7, 10, 1] []

config :: Config
config = defaultConfig

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            (cabalPkg, _) <- loadCabalFromFile config filename
            printCabalPkg cabalPkg
            putStrLn "Parsing...."
            genPkgDesc <- parseCabalFile config filename
            writeCabalFile "/home/mark/test.cabal" genPkgDesc
            putStrLn " Done writing /home/mark/test.cabal"
        _     -> putStrLn "Expected a .cabal filename argument"
                 >> exitFailure
