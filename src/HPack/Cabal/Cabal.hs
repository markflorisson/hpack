{-# LANGUAGE PatternGuards #-}

module HPack.Cabal.Cabal
( CabalPkg(..)
, CabalPkgRef(..)
, VersionRange(..)
, CompilerVersion
, loadCabalFromFile
, printCabalPkg
) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Version (Version(..))
import Control.Monad (mapM)

import qualified Distribution.Package as P
import Distribution.Package (PackageName(..), Dependency(..))
import Distribution.PackageDescription
    (GenericPackageDescription(..), PackageDescription(package)
    , CondTree(..), ConfVar(..), Condition(..), exposedModules)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent, normal, verbose)
import qualified Distribution.Version as V
import Distribution.System (Platform(..), Arch, OS, buildPlatform)
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.ModuleName (components)

import HPack.Source (Pkg(..), ModulePath(..), Version, showVersion)

-- | Elaborate information about the dependencies of a package as parsed
-- from the .cabal file
data CabalPkg
    = CabalPkg
        {
        -- | name of the package
          name           :: String
        -- | version of the package
        , version        :: Version
        -- | dependencies of the library defined in the package (if any)
        , libraryDeps    :: [CabalPkgRef]
        -- | dependencies of any executables defined in the package
        , executableDeps :: [(String, [CabalPkgRef])]
        -- | dependencies used to test the package
        , testDeps       :: [(String, [CabalPkgRef])]
        -- | dependencies used to benchmark the package
        , benchDeps      :: [(String, [CabalPkgRef])]
        -- | modules exposed by the package
        , exposedMods    :: [ModulePath]
        }

-- | Reference to a package through a version range
data CabalPkgRef
    = CabalPkgRef
        { pkgName :: String
        , pkgVersion :: VersionRange
        }

-- | Range of versions
data VersionRange
    = AnyVersion
    | This Version
    | Later Version
    | Earlier Version
    | Union VersionRange VersionRange
    | Intersection VersionRange VersionRange

type CompilerVersion = Version

loadCabalFromFile :: CompilerVersion -> FilePath -> IO CabalPkg
loadCabalFromFile compilerVersion fileName = do
    pkgGenDesc <- parseCabalFile fileName
    let pkgDesc = packageDescription pkgGenDesc
    let name    = P.unPackageName $ P.pkgName $ package pkgDesc
    let version = P.pkgVersion $ package pkgDesc
    let ctx     = Ctx buildPlatform compilerVersion
    return CabalPkg
        { name
            = name
        , version
            = version
        , libraryDeps
            = maybe [] (getDeps ctx) (condLibrary pkgGenDesc)
        , executableDeps
            = [ (s, getDeps ctx d) | (s, d) <- condExecutables pkgGenDesc ]
        , testDeps
            = [ (s, getDeps ctx d) | (s, d) <- condTestSuites pkgGenDesc ]
        , benchDeps
            = [ (s, getDeps ctx d) | (s, d) <- condBenchmarks pkgGenDesc ]
        , exposedMods
            = getExposedModules pkgGenDesc
        }

parseCabalFile :: FilePath -> IO GenericPackageDescription
parseCabalFile = readPackageDescription verbose

---------------------------------------------------------

getExposedModules :: GenericPackageDescription -> [ModulePath]
getExposedModules pkgGenDesc = fromMaybe [] $ do
    CondNode lib _ _ <- condLibrary pkgGenDesc
    return $ map (ModulePath . components) (exposedModules lib)

---------------------------------------------------------

data Ctx = Ctx { platform :: Platform, compilerVersion :: Version}

getDeps :: Ctx -> CondTree ConfVar [Dependency] a -> [CabalPkgRef]
getDeps ctx (CondNode _ deps cs)
        = map convertDeps deps ++ concatMap getChildrenDeps cs
    where
        getChildrenDeps
            :: ( Condition ConfVar
               , CondTree ConfVar [Dependency] a
               , Maybe (CondTree ConfVar [Dependency] a)
               )
            -> [CabalPkgRef]
        getChildrenDeps (cond, condTree, maybeCondTree)
            | evalCond ctx cond
                = getDeps ctx condTree ++
                  maybe [] (getDeps ctx) maybeCondTree
            | otherwise
                = []

evalCond :: Ctx -> Condition ConfVar -> Bool
evalCond ctx cond
    | Var v      <- cond = evalV v
    | Lit b      <- cond = b
    | CNot c     <- cond = not $ evalC c
    | COr c1 c2  <- cond = evalC c1 || evalC c2
    | CAnd c1 c2 <- cond = evalC c1 && evalC c2
    where
        evalC = evalCond ctx
        evalV = evalConfVar ctx

evalConfVar :: Ctx -> ConfVar -> Bool
evalConfVar (Ctx (Platform arch os) _) (OS os')
    = os == os'
evalConfVar (Ctx (Platform arch os) _) (Arch arch')
    = arch == arch'
evalConfVar ctx (Flag flagName)
    = False -- TODO: initialize with defaults
            -- (see https://www.haskell.org/cabal/users-guide/developing-packages.html#executables)
evalConfVar (Ctx _ compilerVersion) (Impl GHC compilerVersionRange)
    = V.withinRange compilerVersion compilerVersionRange
evalConfVar ctx (Impl _ _)
    = False -- we only do GHC for now

---------------------------------------------------------

-- | Convert a Cabal Dependency into a CabalPkgRef that is easier to work with
convertDeps :: Dependency -> CabalPkgRef
convertDeps (Dependency pkgName versionRange)
    = CabalPkgRef (unPackageName pkgName) (convertVersionRange versionRange)

-- | Convert a Cabal version range into our VersionRange
convertVersionRange :: V.VersionRange -> VersionRange
convertVersionRange
    = V.foldVersionRange AnyVersion This Later Earlier Union Intersection

---------------------------------------------------------

printCabalPkg :: CabalPkg -> IO ()
printCabalPkg cabalPkg = do
    putStrLn $ "Name: " ++ name cabalPkg
    putStrLn $ "Version: " ++ show (version cabalPkg)
    putStrLn "Library Dependencies: "
    printDeps $ libraryDeps cabalPkg
    mapM_ (uncurry printExecutable) (executableDeps cabalPkg)
    where
        printDeps = mapM_ printDep
        printDep d = putStrLn $ "    " ++ show d
        printExecutable name deps = do
            putStrLn $ "Executable: " ++ name
            printDeps deps

instance Show CabalPkgRef where
    show (CabalPkgRef name vrange) = name ++ "-" ++ show vrange

instance Show VersionRange where
    show AnyVersion             = "*"
    show (This v)               = showVersion v
    show (Later r)              = "> " ++ show r
    show (Earlier r)            = "< " ++ show r
    show (Union r1 r2)          = show r1 ++ " || " ++ show r2
    show (Intersection r1 r2)   = show r1 ++ " && " ++ show r2
