module HPack.Config
(Config(..), CompilerVersion, defaultConfig)
where

import Data.Version (Version(..))
import Distribution.System
    (Platform, Arch, OS, buildPlatform, buildArch, buildOS)

type CompilerVersion = Version

data Config
    = Config
        { cabalPath       :: FilePath
        , ghcPath         :: FilePath
        , compilerVersion :: CompilerVersion
        , platform        :: Platform
        , arch            :: Arch
        , os              :: OS
        }

defaultConfig :: Config
defaultConfig
    = Config
        { cabalPath       = "cabal"
        , ghcPath         = "ghc"
        , compilerVersion = Version [7, 10, 1] []
        , platform        = buildPlatform
        , arch            = buildArch
        , os              = buildOS
        }
