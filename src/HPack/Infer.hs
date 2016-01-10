{- | Infer interfaces for packages. We do this by proposing a
particular Candidate for linkage with existing compilations of
dependencies. If successful, infer the interfaces of all exposed
modules in the Pkg.

-}
module HPack.Infer
(Candidate(..), tryCandidate)
where

import HPack.Package (Pkg, ModulePath)
import HPack.PkgDB (PkgDB, PkgId)
import HPack.Iface (ModIface(..))


-- | A Candidate proposed for compilation and interface extraction
data Candidate = Candidate Pkg [PkgId]

-- | A mapping from ModulePaths to interface information. Producing
-- this mapping is the main poin
type InterfaceMapping = ModulePath -> ModIface

data CandidateFlags
    = IgnoreCabalUpperBounds
    | IgnoreBadVersion Pkg
    | Combine CandidateFlags CandidateFlags

-- | Given a Pkg, produce all possible candidates that might be
-- considered valid
candidates :: PkgDB -> CandidateFlags -> Pkg -> [Candidate]
candidates = undefined

-- | Try a Candidate and return a new PkgId for the compiled package
-- if successful, along with a mapping from modules to interface information
tryCandidate :: PkgDB -> Candidate -> IO (Maybe (PkgId, InterfaceMapping))
tryCandidate = undefined
