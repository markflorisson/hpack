module HPack.Package
( PkgInfo(..)
, Pkg(..)
, PkgRef(..)
, PkgSource(..)
, ModulePath(..)
, VersionRange(..)
, printPkgInfo
, showVersion
) where

import Data.List (intercalate)
import Data.Version (Version(..))

-- | Elaborrate information about a package's dependencies, e.g. as
-- parsed from a .cabal file
data PkgInfo
    = PkgInfo
        {
        -- | name of the package
          name           :: String
        -- | version of the package
        , version        :: Version
        -- | dependencies of the library defined in the package (if any)
        , libraryDeps    :: [PkgRef]
        -- | dependencies of any executables defined in the package
        , executableDeps :: [(String, [PkgRef])]
        -- | dependencies used to test the package
        , testDeps       :: [(String, [PkgRef])]
        -- | dependencies used to benchmark the package
        , benchDeps      :: [(String, [PkgRef])]
        -- | modules exposed by the package
        , exposedMods    :: [ModulePath]
        }

-- | A package name and version pair
data Pkg        = Pkg String Version

-- | Reference to a package through a version range
-- (e.g. from a .cabal file)
data PkgRef     = PkgRef { pkgName :: String, pkgVersion :: VersionRange }

-- | Path to a module within some package
data ModulePath = ModulePath [String]

-- | Location of the source code for a given package
data PkgSource = PkgSource Pkg FilePath


-- | Range of versions
data VersionRange
    = AnyVersion
    | This Version
    | Later Version
    | Earlier Version
    | Union VersionRange VersionRange
    | Intersection VersionRange VersionRange

---------------------------------------------------------

printPkgInfo :: PkgInfo -> IO ()
printPkgInfo pkgInfo = do
    putStrLn $ "Name: " ++ name pkgInfo
    putStrLn $ "Version: " ++ show (version pkgInfo)
    putStrLn "Library Dependencies: "
    printDeps $ libraryDeps pkgInfo
    mapM_ (uncurry printExecutable) (executableDeps pkgInfo)
    where
        printDeps = mapM_ printDep
        printDep d = putStrLn $ "    " ++ show d
        printExecutable name deps = do
            putStrLn $ "Executable: " ++ name
            printDeps deps

instance Show Pkg where
    show (Pkg name version) = name ++ "-" ++ showVersion version

instance Show PkgRef where
    show (PkgRef name vrange) = name ++ "-" ++ show vrange


instance Show VersionRange where
    show AnyVersion             = "*"
    show (This v)               = showVersion v
    show (Later r)              = "> " ++ show r
    show (Earlier r)            = "< " ++ show r
    show (Union r1 r2)          = show r1 ++ " || " ++ show r2
    show (Intersection r1 r2)   = show r1 ++ " && " ++ show r2


showVersion :: Version -> String
showVersion (Version versionInfo tags)
    = intercalate "." (map show versionInfo ++ tags)
