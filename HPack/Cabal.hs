{-# LANGUAGE PatternGuards #-}

module HPack.Cabal
( PkgInfo(..)
, Pkg(..)
, PkgRef(..)
, ModulePath(..)
, VersionRange(..)
, loadCabalFile
, printPkgInfo
) where

import System.FilePath ((</>))
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

data Pkg        = Pkg String Version
data PkgRef     = PkgRef { pkgName :: String, pkgVersion :: VersionRange }
data ModulePath = ModulePath [String]

data VersionRange
    = AnyVersion
    | This Version
    | Later Version
    | Earlier Version
    | Union VersionRange VersionRange
    | Intersection VersionRange VersionRange

loadCabalFile :: Version -> Pkg -> IO PkgInfo
loadCabalFile compilerVersion pkg = do -- fmap getLibraryDeps . parseCabalFile
    let fileName = getCabalFilename pkg
    pkgGenDesc <- parseCabalFile fileName
    let pkgDesc = packageDescription pkgGenDesc
    let name    = P.unPackageName $ P.pkgName $ package pkgDesc
    let version = P.pkgVersion $ package pkgDesc
    let ctx     = Ctx buildPlatform compilerVersion
    return PkgInfo
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

getCabalFilename :: Pkg -> FilePath
getCabalFilename (Pkg name version)
    = "metadata" </> name </> showVersion version

parseCabalFile :: FilePath -> IO GenericPackageDescription
parseCabalFile = readPackageDescription verbose

---------------------------------------------------------

getExposedModules :: GenericPackageDescription -> [ModulePath]
getExposedModules pkgGenDesc = fromMaybe [] $ do
    CondNode lib _ _ <- condLibrary pkgGenDesc
    return $ map (ModulePath . components) (exposedModules lib)

---------------------------------------------------------

data Ctx = Ctx { platform :: Platform, compilerVersion :: Version}

getDeps :: Ctx -> CondTree ConfVar [Dependency] a -> [PkgRef]
getDeps ctx (CondNode _ deps cs)
        = map convertDeps deps ++ concatMap getChildrenDeps cs
    where
        getChildrenDeps
            :: ( Condition ConfVar
               , CondTree ConfVar [Dependency] a
               , Maybe (CondTree ConfVar [Dependency] a)
               )
            -> [PkgRef]
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
    = False -- TODO
evalConfVar (Ctx _ compilerVersion) (Impl GHC compilerVersionRange)
    = V.withinRange compilerVersion compilerVersionRange
evalConfVar ctx (Impl _ _)
    = False -- we only do GHC for now

---------------------------------------------------------

-- | Convert a Cabal Dependency into a Pkg
convertDeps :: Dependency -> PkgRef
convertDeps (Dependency pkgName versionRange)
    = PkgRef (unPackageName pkgName) (convertVersionRange versionRange)

-- | Convert a Cabal version range into our VersionRange
convertVersionRange :: V.VersionRange -> VersionRange
convertVersionRange = V.foldVersionRange AnyVersion This Later Earlier Union Intersection

---------------------------------------------------------

printPkgInfo :: PkgInfo -> IO ()
printPkgInfo pkgInfo = do
    putStrLn $ "Name: " ++ name pkgInfo
    putStrLn $ "Version: " ++ show (version pkgInfo)
    putStrLn $ "Library Dependencies: "
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
