module HPack.Solver.DepGraph
(DepGraph, TopoSort(..), builtinPkgs, loadGraph, union) -- , toposort)
where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad (forM, foldM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

import HPack.Source (Pkg(..))
import HPack.Cabal
    ( CabalRepo, CabalRepoM, CabalError, CabalPkg(..), CabalPkgRef(..)
    , runCabalRepoM, loadCabalFromPkg, listCabalPkgs, withinRange
    )
import HPack.Config (Config(..))

data TopoSort = TopoSort { solved :: [Pkg], unsolved :: [Pkg] }

-- | Dependency graph mapping. Each node is a Pkg, and each
-- edge is represented by a list of disjuctions of package versions.
-- E.g.
--
--      A-0.2.0 -> Disj [B-0.3.1, B-0.3.2, ...]
--
data DepGraph = DepGraph (M.Map Pkg [Disj])

data Disj = Disj [Pkg]

union :: DepGraph -> DepGraph -> DepGraph
union (DepGraph g1) (DepGraph g2) = DepGraph (g1 `M.union` g2)

-- | Packages built into GHC
builtinPkgs :: S.Set Pkg
builtinPkgs = S.empty

-- | Load a version-based dependency graph from .cabal files
loadGraph :: CabalRepo -> Config -> Pkg -> IO (Either CabalError DepGraph)
loadGraph cabalRepo config pkg
    = runCabalRepoM $ loadGraph' pkg (DepGraph M.empty)
    where
        -- | Compute the transitive closure all dependencies as a
        -- DepGraph
        loadGraph' :: Pkg -> DepGraph -> CabalRepoM IO DepGraph
        loadGraph' pkg (DepGraph edges) = do
            deps <- pkgDeps pkg
            let graph = DepGraph $ M.insert pkg (map Disj deps) edges
            foldM maybeLoadGraph graph (concat deps)

        -- | Find all immediate package dependencies for a Pkg
        pkgDeps :: Pkg -> CabalRepoM IO [[Pkg]]
        pkgDeps pkg@(Pkg name version) = do
            -- Load .cabal file for `pkg`
            CabalPkg{libraryDeps = libDeps, executableDeps = execDeps}
                <- loadCabalFromPkg cabalRepo config pkg
            -- find dependencies for package from .cabal file
            let deps = libDeps ++ concatMap snd execDeps
            liftIO $ forM deps $ \(CabalPkgRef depName depVersionRange) -> do
                pkgCandidates <- listCabalPkgs cabalRepo depName
                return [ Pkg n v | (Pkg n v) <- pkgCandidates
                                 , withinRange v depVersionRange ]

        -- | Skip loading things that are already loaded
        maybeLoadGraph :: DepGraph -> Pkg -> CabalRepoM IO DepGraph
        maybeLoadGraph graph@(DepGraph edges) pkg
            = if M.member pkg edges
                  then return graph
                  else loadGraph' pkg graph

{- | Do a topological sort on the dependency graph. Note that the
     dependency graph is based on version ranges, represented by
     disjunctions, e.g.

        A-0.2.0 -> Disj [B-0.3.1, B-0.3.2, ...]

     Here we try to order A-0.2.0 after all of B-0.3.1, B-0.3.2, ...
     However, some package might not be topologically sortable,
     in case of a cycle. A cycle might be

        B-0.3.1 -> C-0.1
        C-0.1   -> B-0.3.1

     but another form is cycle is the following:

        B-0.3.1 -> C-0.1 or C-0.2
        C-0.2   -> B-0.3.1

     here a non-recursive sorting could be

        [C-0.1, B-0.3.1, C-0.2]

     but we do not find such solutions. Instead we find

        solved   = [C-0.1, B-0.3.1]
        unsolved = [C-0.2]

     we do this as follows: whenever we cannot by finding candidates
-}
-- toposort :: DepGraph -> TopoSort
-- toposort graph@(DepGraph edges)
--     | solved   <- topo builtinPkgs
--     , unsolved <- S.fromList (M.keys edges) S.\\ solved
--         = TopoSort solved (S.toList unsolved)
--     where
--         topo :: S.Set Pkg -> [Pkg]
--         topo solved
--             | candidates <- getCandidates solved
--             , not (null candidates)
--                 = candidates ++ topo (solved `S.union` S.fromList candidates)
--             | otherwise
--                 = []
--
--         getCandidates :: S.Set Pkg -> [Pkg]
--         getCandidates solved = go 0
--             where
--                 go n
--                     | cs <- candidates n
--                     , not (null cs)
--                         = cs
--                     | n < maxDeps
--                         = go (n+1)
--                     | otherwise
--                         = []
--
--                 members deps = filter (flip S.member solved) deps
--                 candidates nDepsMissing =
--                     [ pkg | (pkg, Disj deps) <- M.assocs edges
--                           , length (members deps) > (length deps) - nDepsMissing
--                           ]
--
--         maxDeps :: Int
--         maxDeps = max [ length deps | Disj deps <- M.elems edges ]
