{-# LANGUAGE PatternGuards #-}

module HPack.Cabal
(CompilerVersion, loadCabalFromPkg, loadCabalFromFile)
where

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

import HPack.Package
    ( PkgInfo(..), Pkg(..), PkgRef(..), ModulePath(..), VersionRange(..)
    , showVersion
    )

type CompilerVersion = Version

loadCabalFromPkg :: CompilerVersion -> Pkg -> IO PkgInfo
loadCabalFromPkg compilerVersion pkg
    = loadCabalFromFile compilerVersion (getCabalFilename pkg)

loadCabalFromFile :: CompilerVersion -> FilePath -> IO PkgInfo
loadCabalFromFile compilerVersion fileName = do
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
    = False -- TODO: initialize with defaults
            -- (see https://www.haskell.org/cabal/users-guide/developing-packages.html#executables)
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
