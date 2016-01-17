{- | Infer interfaces for packages by proposing a
particular Candidate for linkage with existing compilations of
dependencies. If successful, infer the interfaces of all exposed
modules in the Pkg.

-}
module HPack.System.Plan
(BuildPlan(..), computeAllPlans)
where

import HPack.Source (Pkg, ModulePath)
import HPack.System.PkgDB (PkgDB, PkgId)
import HPack.Infer.Iface (ModIface(..))

-- | A possible plan to be proposed for compilation and interface extraction
data BuildPlan = BuildPlan Pkg [PkgId]

-- | A mapping from ModulePaths to interface information. Producing
-- this mapping is the main poin
type InterfaceMapping = ModulePath -> ModIface

data CandidateFlags
    = IgnoreCabalUpperBounds
    | IgnoreBadVersion Pkg
    | Combine CandidateFlags CandidateFlags

-- | Given a Pkg, produce all possible build plans that might be
-- considered valid
computeAllPlans :: PkgDB -> CandidateFlags -> Pkg -> [BuildPlan]
computeAllPlans = undefined
