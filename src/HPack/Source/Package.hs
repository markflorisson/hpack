module HPack.Source.Package
( Pkg(..)
, Version(..)
, ModulePath(..)
, showVersion
) where

import Data.List (intercalate)
import Data.Version (Version(..))


-- | A package name and version pair
data Pkg = Pkg String Version
    deriving (Eq, Ord)

-- | Path to a module within some package
data ModulePath = ModulePath [String]
    deriving (Eq, Ord)

---------------------------------------------------------

instance Show Pkg where
    show (Pkg name version) = name ++ "-" ++ showVersion version

showVersion :: Version -> String
showVersion (Version versionInfo tags)
    = intercalate "." (map show versionInfo ++ tags)
