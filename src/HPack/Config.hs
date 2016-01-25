module HPack.Config
(Config(..), CompilerVersion, defaultConfig)
where

import Data.Version (Version(..))

type CompilerVersion = Version

data Config
    = Config
        { cabalPath       :: FilePath
        , ghcPath         :: FilePath
        , compilerVersion :: CompilerVersion
        }

defaultConfig :: Config
defaultConfig
    = Config
        { cabalPath = "cabal"
        , ghcPath = "ghc"
        , compilerVersion = Version [7, 10, 1] []
        }
