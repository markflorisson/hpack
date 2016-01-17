module HPack.Source.SourceRepo
(SourceRepo, initSourceRepo, destroySourceRepo, getSourceForPkg)
where

import HPack.Source.Package (Pkg)

-- | An on-disk repository of Haskell source code, as downloaded from
-- hackage.
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
getSourceForPkg :: SourceRepo -> Pkg -> IO (Either IOError FilePath)
getSourceForPkg = undefined
