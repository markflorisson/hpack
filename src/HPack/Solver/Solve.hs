{-# LANGUAGE NoMonomorphismRestriction #-}
module HPack.Solver.Solve
(SolverM, runSolverM, solve, SolverFlags(..))
where

import qualified Data.Map as M

import HPack.Source (Pkg)
import HPack.Cabal (CabalRepo)
import HPack.System (PkgDB, PkgId)
import HPack.Solver.DepGraph (DepGraph)
import HPack.Monads

type SolverM m a = HPackT m a

runSolverM = runHPackT

-- | Flags for package resolution
data SolverFlags
    = CandiateFlags
        { ignoreUpperBounds :: Bool
        , badPackages       :: [Pkg]
        }

solve :: SolverFlags -> Pkg -> DepGraph -> PkgDB -> PkgDB
solve = undefined
