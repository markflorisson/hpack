module HPack.Sandbox where

import HPack.PkgDB (PkgDB, PkgId)

import Control.Monad.Trans.State

{-
data SandboxState = Sandbox FilePath PkgSet

type Sandbox = StateT SandboxState IO

runSandbox :: Sandbox a -> (SandboxState, a)

createSandbox :: FilePath -> Sandbox ()
registerPkgSet :: PkgSet -> Sandbox ()
registerPkg :: PkgId -> Sandbox ()

-- | Try to compile the package in the (appropriately initialized) sandbox.
-- On success, returns a new PkgId for the package along with an updated
-- PkgSet
tryCompile :: Pkg -> Sandbox PkgId
-}
