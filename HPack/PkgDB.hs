module HPack.PkgDB where

import Data.Map (Map, lookup, insert)
import HPack.Package (PkgInfo(..), Pkg(..), ModulePath, printPkgInfo)
import HPack.Iface (ModIface(..))

newtype PkgId   = PkgId Int
newtype PkgHash = PkgHash Int
type PkgSet     = [(PkgId, PkgId)]

data Candidate = Candidate Pkg PkgSet

data PkgDB = PkgDB
    { pkgInfo  :: Map PkgId Pkg
    , depInfo  :: Map PkgId [PkgId]
    , fileInfo :: Map PkgId PkgHash
    }

candidates :: Pkg -> PkgSet
candidates = undefined

-- | Try a Candidate and return a new PkgId for the compiled package
-- if successful, along with a mapping from modules to interface information
tryCandidate :: Candidate -> IO (Maybe (PkgId, ModulePath -> ModIface))
tryCandidate = undefined
