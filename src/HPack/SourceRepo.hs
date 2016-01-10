module HPack.SourceRepo
(SourceRepo, getSourceForPkg)
where

import HPack.Package (Pkg, PkgSource)

data SourceRepo = SourceRepo FilePath

-- | Initialize repository of source code
initSourceRepo :: FilePath -> IO SourceRepo
initSourceRepo = undefined

-- | Wipe out the source repository
destroySourceRepo :: SourceRepo -> IO ()
destroySourceRepo = undefined

-- | Get the source location for a given package.
-- If not present in the source repository, cabal fetch/cabal get
-- the sources for pkg-version
getSourceForPkg :: SourceRepo -> Pkg -> IO FilePath
getSourceForPkg = undefined
