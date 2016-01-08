{-# LANGUAGE RecordWildCards #-}

module HPack.PkgDB where

import Data.Map (Map, lookup, insert)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, runStateT, put, get, modify)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, catchE)

import HPack.Package (PkgInfo(..), Pkg(..), PkgSource(..)
                     , ModulePath, printPkgInfo)
import HPack.Iface (ModIface(..))

data DBErr
    -- | some IOError has occurred
    = DBInitError IOError

type DB = ExceptT DBErr (StateT PkgDB IO)

-- | A concrete package *instantiation*: that is, a particular
-- compilation and linkage of a package. Also used to store the
-- package on disk in the PkgDB
newtype PkgId = PkgId Int
    -- this should probably not be an int but a SHA256 hash or something
    deriving (Ord, Eq, Show)

data PkgDB = PkgDB
    { dbLocation :: FilePath          -- ^ package db location on disk
    , pkgInfo    :: Map PkgId Pkg     -- ^ package name and version for
                                      --   each package ID
    , depInfo    :: Map PkgId [PkgId] -- ^ dependencies for package ID
    }

---------------------------------------------------------

-- | Create initial (empty) package database
initPkgDB :: FilePath -> DB ()
initPkgDB = undefined

-- | Load package database from disk
loadPkgDB :: FilePath -> DB ()
loadPkgDB = undefined

-- | Save package database to disk (to dbLocation)
savePkgDB :: DB ()
savePkgDB = undefined

---------------------------------------------------------

-- | Try to compile the package against the given dependencies.
-- On success, returns a new PkgId for the package, and update the
-- PkgDB
tryCompile :: PkgSource -> [PkgId] -> DB PkgId
tryCompile (PkgSource pkg pkgLocation) dependencies = do
    -- 1) allocate temporary directory
    tmpDir <- undefined

    -- 2) copy package sources into temporary directory

    -- 3) replace cabal file with specific package versions to match
    --    the versions from Candidate
    --    (this may be the easiest way to "fix" the versions of the
    --     dependencies, but maybe there is another way)

    -- 4) cabal build the package

    -- 5) compute package hash
    pkgId <- computePkgHash pkg dependencies

    -- 6) move tmpDir to dbLocation/pkgId
    --    (move package under its proper hash in the package database)

    -- 7) if successful, update PkgDB
    PkgDB {..} <- lift get
    let pkgInfo' = insert pkgId pkg pkgInfo
    let depInfo' = insert pkgId dependencies depInfo
    lift $ modify (\pkgDB -> PkgDB dbLocation pkgInfo' depInfo')

    -- 8) register the compiled package with GHC's database
    -- ghc-pkg register / cabal install

    -- 9) done :)
    return pkgId

-- | Compute the hash for the package, given its name, version and
-- a list of its compiled dependencies
computePkgHash :: Pkg -> [PkgId] -> DB PkgId
computePkgHash pkg dependencies = undefined

-- | Find all existing compilations in the package database for the
-- given package (useful to figure out what compilations to propose)
findCompilations :: PkgDB -> Pkg -> [PkgId]
findCompilations = undefined
