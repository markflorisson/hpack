module HPack.Solver.Solve
(solve)
where

import qualified Data.Map as M

import HPack.Source (Pkg)
import HPack.Cabal (CabalRepo)
import HPack.System (PkgDB, PkgId)
import HPack.Infer (IfaceRepo)

solve :: Pkg -> CabalRepo -> IfaceRepo -> PkgDB -> PkgDB
solve = undefined
