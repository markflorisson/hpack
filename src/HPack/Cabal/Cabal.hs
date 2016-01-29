{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

module HPack.Cabal.Cabal
( CabalPkg(..)
, CabalPkgRef(..)
, VersionRange(..)
, CompilerVersion
, GenericPackageDescription
, parseCabalFile
, loadCabalFromFile
, writeCabalFile
, printCabalPkg
, withinRange
) where

import Data.Maybe (mapMaybe, catMaybes)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Version (Version(..))
import Control.Monad (mapM)
import Control.DeepSeq (deepseq)

import System.IO (IOMode(..), withFile, hPutStr, hPutStrLn, hPrint)
import qualified Distribution.Package as P
import Distribution.Package (PackageName(..), Dependency(..))
import Distribution.PackageDescription
    ( GenericPackageDescription(..), PackageDescription(..)
    , CondTree(..), ConfVar(..), Condition(..), Library
    , exposedModules
    )
import Distribution.PackageDescription.Parse
    (readPackageDescription, writePackageDescription)
import Distribution.Verbosity (silent, normal, verbose)
import qualified Distribution.Version as V
import Distribution.System (Platform(..), Arch, OS, buildPlatform)
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.ModuleName (components)

import HPack.Config (Config(..), CompilerVersion)
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

type VersionRange = V.VersionRange


data CabalCtx = CabalCtx { platform :: Platform, config :: Config }

type CabalTree = CondTree ConfVar [Dependency]
type CabalCond = Condition ConfVar


withinRange = V.withinRange

loadCabalFromFile :: Config -> FilePath -> IO CabalPkg
loadCabalFromFile config fileName = do
    pkgGenDesc <- parseCabalFile fileName
    let pkgDesc = packageDescription pkgGenDesc
    let name    = P.unPackageName $ P.pkgName $ package pkgDesc
    let version = P.pkgVersion $ package pkgDesc
    let ctx     = CabalCtx buildPlatform config
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
parseCabalFile filename = do
    brokenGenPkgDesc <- readPackageDescription verbose filename
    let brokenPkgDesc = packageDescription brokenGenPkgDesc

    let condTree = condLibrary brokenGenPkgDesc
    let maybeLibrary = fmap condTreeData condTree
    let pkgDesc = brokenPkgDesc { library = maybeLibrary }

    return $ brokenGenPkgDesc { packageDescription = pkgDesc }

writeCabalFile :: FilePath
               -> GenericPackageDescription
               -> CabalPkg
               -> IO ()
writeCabalFile filename genPkgDesc CabalPkg{..} = do
    writePackageDescription filename (packageDescription genPkgDesc)

    -- withFile filename WriteMode $ \h -> do
    --     hPutStr   h "name: "
    --     hPutStrLn h name
    --     hPutStr   h "version: "
    --     hPutStrLn h (showVersion version)
    --
    --     hPutStrLn h "library"
    --     do
    --         hPutStrLn h "hs-source-dirs"
    --         hPutStrLn h "exposed-modules:"

-- | Update the version constraints for package dependencies contained
-- in GenericPackageDescription with the versions from the given packages
fixPackageVersions :: [Pkg]
                   -> GenericPackageDescription
                   -> GenericPackageDescription
fixPackageVersions pkgs pkgDescription
    = pkgDescription { condLibrary = libraryDeps }
    where
        libraryDeps :: Maybe (CabalTree Library)
        libraryDeps
            | Just deps <- condLibrary pkgDescription
                = Just (mapTree (map transformDependency) id deps)
            | otherwise
                = Nothing

        transformDependency :: Dependency -> Dependency
        transformDependency dep
            | Dependency (PackageName pkgName) versionRange <- dep
            , (pkg:_) <- filter (\(Pkg pkgName' _) -> pkgName == pkgName') pkgs
                = fixedDependency pkg
            | otherwise
                = dep

        fixedDependency :: Pkg -> P.Dependency
        fixedDependency (Pkg name version)
            = Dependency (PackageName name) (V.thisVersion version)

---------------------------------------------------------

getExposedModules :: GenericPackageDescription -> [ModulePath]
getExposedModules pkgGenDesc = fromMaybe [] $ do
    CondNode lib _ _ <- condLibrary pkgGenDesc
    return $ map (ModulePath . components) (exposedModules lib)

---------------------------------------------------------

-- | Map a bunch of functions over the rather brutal CondTree
mapTree :: ([Dependency] -> [Dependency])
        -> (a -> a)
        -> CabalTree a
        -> CabalTree a
mapTree f g (CondNode treeData treeConstraints treeComponents)
    = CondNode (g treeData)
               (f treeConstraints)
               [ ( condition
                 , mapTree f g tree
                 , maybeMap maybeTree
                 ) | (condition, tree, maybeTree) <- treeComponents
               ]
    where
        maybeMap Nothing     = Nothing
        maybeMap (Just tree) = Just (mapTree f g tree)

-- | Fold over the data of a CondTree ('Library', 'Benchmark',
-- 'Executabe', or 'TestSuite')
foldTree :: (a -> b -> b) -> b -> CondTree v c a -> b
foldTree f z tree
    = foldr f z (values tree)
    where
        values (CondNode treeData treeConstraints treeComponents)
            = treeData : concatMap extract treeComponents

        extract (_, tree, maybeTree) =
            values tree ++ maybe [] values maybeTree

-- | Get all dependencies from the given CabalTree
getDeps :: CabalCtx -> CabalTree a -> [CabalPkgRef]
getDeps ctx (CondNode _ deps cs)
        = map convertDep deps ++ concatMap getChildrenDeps cs
    where
        getChildrenDeps (cond, tree, maybeTree)
                = getDeps ctx tree ++
                  maybe [] (getDeps ctx) maybeTree

        convertDep :: Dependency -> CabalPkgRef
        convertDep (Dependency pkgName versionRange)
            = CabalPkgRef (unPackageName pkgName) versionRange

-- | Prune any subtrees that don't satisfy the cabal conditions
-- as specified in the .cabal file (platform, architecture, etc)
pruneTree :: CabalCtx -> CabalTree a -> CabalTree a
pruneTree ctx (CondNode treeData treeConstraints treeComponents) =
    let cs = catMaybes (map get treeComponents)
    in  CondNode treeData treeConstraints cs
    where
        get :: (CabalCond, CabalTree a, Maybe (CabalTree a))
            -> Maybe (CabalCond, CabalTree a, Maybe (CabalTree a))
        get (cond, tree, maybeTree)
            | evalCond ctx cond
            , tree' <- pruneTree ctx tree
            , maybeTree' <- fmap (pruneTree ctx) maybeTree
                = Just (cond, tree', maybeTree')
            | otherwise
                = Nothing

evalCond :: CabalCtx -> CabalCond -> Bool
evalCond ctx cond
    | Var v      <- cond = evalV v
    | Lit b      <- cond = b
    | CNot c     <- cond = not $ evalC c
    | COr c1 c2  <- cond = evalC c1 || evalC c2
    | CAnd c1 c2 <- cond = evalC c1 && evalC c2
    where
        evalC = evalCond ctx
        evalV = evalConfVar ctx

evalConfVar :: CabalCtx -> ConfVar -> Bool
evalConfVar (CabalCtx (Platform arch os) _) (OS os')
    = os == os'
evalConfVar (CabalCtx (Platform arch os) _) (Arch arch')
    = arch == arch'
evalConfVar ctx (Flag flagName)
    = False -- TODO: initialize with defaults
            -- (see https://www.haskell.org/cabal/users-guide/developing-packages.html#executables)
evalConfVar (CabalCtx _ config) (Impl GHC compilerVersionRange)
    = V.withinRange (compilerVersion config) compilerVersionRange
evalConfVar ctx (Impl _ _)
    = False -- we only do GHC for now

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
