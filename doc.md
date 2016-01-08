# GHC API

[Using the GHC API](https://wiki.haskell.org/GHC/As_a_library)

[The GHC API Implementation](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/API)

The GHC API uses a Session value to ensure the linker is accessed only from
a single thread. This Session value is kept in a monad instance of the
typeclass ``GhcMonad``. Two implementations are ``Ghc`` and ``GhcT`` (the
``Ghc`` monad transformer).

## Compiler Flags (DynFlags)

- HscInterpreted means you can still get TemplateHaskell support but you're
    not generating output code
- LinkInMemory is required to be able to reload modules properly.
    Using NoLink will break that: reloading a module won't allow allow
    to run interactive statements on the new version of the module
- CompManager ensures dependent modules are built and loaded if needed.

## GHC documentation

[GHC Docs](https://ghc.haskell.org/trac/ghc/wiki/Commentary)

# Packages

[Packages and ABI](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages)

## Architecture

GHC maintains a package database with ``ghc-pkg`` (to register/unregister
and manage the package database). ``ghc-pkg`` itself relies on Cabal, which
defines the various data types used by the package system. The data type
for the package database is defined in Cabal in
``Distribution.InstalledPackageInfo``. The Cabal sources can be found
in ``libraries/Cabal``.

GHC uses the package database to find ``.hi`` files and link libraries,
and also depends on Cabal. ``cabal-install`` also depends on Cabal, and
adds support for downloading and installing packages from Hackage.

The file `` compiler/main/HscTypes.lhs`` contains a bunch of data type
definitions and functions for compiling packages. Before linking the
a module has an interface described by ``ModIface``, and after linking
an interface described by ``ModDetails``.

## The GHC API

The GHC API (that you can import from Haskell code) is defined in
``compiler/main/GHC.hs``. The ``ModGuts`` type carries information about
the module that is being compiled (there is only one active at a time).
The ``ModGuts`` is then converted to a ``ModIface`` and a ``ModDetails``
after compilation.

    modInfoIface :: ModuleInfo -> Maybe ModIface

So we can extract the ``ModIface`` from a ``ModuleInfo``, which we can
extract from the ``Module``:

    getModuleInfo        :: GhcMonad m => Module -> m (Maybe ModuleInfo)  -- XXX: Maybe X

``ModuleInfo`` contains the following information:

    data ModuleInfo = ModuleInfo {
        minf_type_env  :: TypeEnv,
        minf_exports   :: [AvailInfo],
        minf_rdr_env   :: Maybe GlobalRdrEnv,   -- Nothing for a compiled/package mod
        minf_instances :: [ClsInst],
        minf_iface     :: Maybe ModIface,
        minf_safe      :: SafeHaskellMode
\#ifdef GHCI
       ,minf_modBreaks :: ModBreaks
\#endif
    }

We now need to get at a ``Module`` (see ``GHC.hs``):

    -- | Takes a 'ModuleName' and possibly a 'UnitId', and consults the
    -- filesystem and package database to find the corresponding 'Module',
    -- using the algorithm that is used for an @import@ declaration.
    findModule :: GhcMonad m => ModuleName -> Maybe FastString -> m Module

    mkModuleName :: String -> ModuleName
    moduleName :: ModuleName -> String

### ModIface
We now have a ``ModIface``, which is defined in ``compiler/main/HscTypes.hs``
(shown below). ``compiler/iface/LoadIface.hs`` defines a bunch of utilities
for using ``ModIface``, such as ``pprModIface``.

    -- | A 'ModIface' plus a 'ModDetails' summarises everything we know
    -- about a compiled module.  The 'ModIface' is the stuff *before* linking,
    -- and can be written out to an interface file. The 'ModDetails is after
    -- linking and can be completely recovered from just the 'ModIface'.
    --
    -- When we read an interface file, we also construct a 'ModIface' from it,
    -- except that we explicitly make the 'mi_decls' and a few other fields empty;
    -- as when reading we consolidate the declarations etc. into a number of indexed
    -- maps and environments in the 'ExternalPackageState'.
    data ModIface
      = ModIface {
            mi_module     :: !Module,             -- ^ Name of the module we are for
            mi_sig_of     :: !(Maybe Module),     -- ^ Are we a sig of another mod?
            mi_iface_hash :: !Fingerprint,        -- ^ Hash of the whole interface
            mi_mod_hash   :: !Fingerprint,        -- ^ Hash of the ABI only
            mi_flag_hash  :: !Fingerprint,        -- ^ Hash of the important flags
                                                  -- used when compiling this module

            mi_orphan     :: !WhetherHasOrphans,  -- ^ Whether this module has orphans
            mi_finsts     :: !WhetherHasFamInst,  -- ^ Whether this module has family instances
            mi_hsc_src    :: !HscSource,          -- ^ Boot? Signature?

            mi_deps     :: Dependencies,
                    -- ^ The dependencies of the module.  This is
                    -- consulted for directly-imported modules, but not
                    -- for anything else (hence lazy)

            mi_usages   :: [Usage],
                    -- ^ Usages; kept sorted so that it's easy to decide
                    -- whether to write a new iface file (changing usages
                    -- doesn't affect the hash of this module)
                    -- NOT STRICT!  we read this field lazily from the interface file
                    -- It is *only* consulted by the recompilation checker

            mi_exports  :: ![IfaceExport],
                    -- ^ Exports
                    -- Kept sorted by (mod,occ), to make version comparisons easier
                    -- Records the modules that are the declaration points for things
                    -- exported by this module, and the 'OccName's of those things

            mi_exp_hash :: !Fingerprint,
                    -- ^ Hash of export list

            mi_used_th  :: !Bool,
                    -- ^ Module required TH splices when it was compiled.
                    -- This disables recompilation avoidance (see #481).

            mi_fixities :: [(OccName,Fixity)],
                    -- ^ Fixities
                    -- NOT STRICT!  we read this field lazily from the interface file

            mi_warns    :: Warnings,
                    -- ^ Warnings
                    -- NOT STRICT!  we read this field lazily from the interface file

            mi_anns     :: [IfaceAnnotation],
                    -- ^ Annotations
                    -- NOT STRICT!  we read this field lazily from the interface file


            mi_decls    :: [(Fingerprint,IfaceDecl)],
                    -- ^ Type, class and variable declarations
                    -- The hash of an Id changes if its fixity or deprecations change
                    --      (as well as its type of course)
                    -- Ditto data constructors, class operations, except that
                    -- the hash of the parent class/tycon changes

            mi_globals  :: !(Maybe GlobalRdrEnv),
                    -- ^ Binds all the things defined at the top level in
                    -- the /original source/ code for this module. which
                    -- is NOT the same as mi_exports, nor mi_decls (which
                    -- may contains declarations for things not actually
                    -- defined by the user).  Used for GHCi and for inspecting
                    -- the contents of modules via the GHC API only.
                    --
                    -- (We need the source file to figure out the
                    -- top-level environment, if we didn't compile this module
                    -- from source then this field contains @Nothing@).
                    --
                    -- Strictly speaking this field should live in the
                    -- 'HomeModInfo', but that leads to more plumbing.

                    -- Instance declarations and rules
            mi_insts       :: [IfaceClsInst],     -- ^ Sorted class instance
            mi_fam_insts   :: [IfaceFamInst],  -- ^ Sorted family instances
            mi_rules       :: [IfaceRule],     -- ^ Sorted rules
            mi_orphan_hash :: !Fingerprint,    -- ^ Hash for orphan rules, class and family
                                               -- instances, and vectorise pragmas combined

            mi_vect_info :: !IfaceVectInfo,    -- ^ Vectorisation information

                    -- Cached environments for easy lookup
                    -- These are computed (lazily) from other fields
                    -- and are not put into the interface file
            mi_warn_fn   :: OccName -> Maybe WarningTxt,
                    -- ^ Cached lookup for 'mi_warns'
            mi_fix_fn    :: OccName -> Fixity,
                    -- ^ Cached lookup for 'mi_fixities'
            mi_hash_fn   :: OccName -> Maybe (OccName, Fingerprint),
                    -- ^ Cached lookup for 'mi_decls'.
                    -- The @Nothing@ in 'mi_hash_fn' means that the thing
                    -- isn't in decls. It's useful to know that when
                    -- seeing if we are up to date wrt. the old interface.
                    -- The 'OccName' is the parent of the name, if it has one.

            mi_hpc       :: !AnyHpcUsage,
                    -- ^ True if this program uses Hpc at any point in the program.

            mi_trust     :: !IfaceTrustInfo,
                    -- ^ Safe Haskell Trust information for this module.

            mi_trust_pkg :: !Bool
                    -- ^ Do we require the package this module resides in be trusted
                    -- to trust this module? This is used for the situation where a
                    -- module is Safe (so doesn't require the package be trusted
                    -- itself) but imports some trustworthy modules from its own
                    -- package (which does require its own package be trusted).
                    -- See Note [RnNames . Trust Own Package]
         }

### Type Checking Monads
``typecheck/TcRnTypes`` contains the ``TcM`` and ``TcRnMonad``
monads, also accessible through ``typecheck/TcRnMonad``.

- ``RnM``: renaming monad (``RnM = TcRn``)
- ``TcM``: type-checking monad (``TcM = TcRn``)
- ``IfM``: interface monad

During type-checking the ``TcRn`` monad keeps track of a global environment
that contains all the top-level definitions (``TcGblEnv``), and a local
environment that is updated as expressions are analyzed (``TcLclEnv``).
Environments are stacked in a ``Reader``-monad like fashion (at least for
the local environment). During interface construction, the process starts
in the ``IfG`` monad ("iface global"), with an empty local environment,
and proceeds with the ``IfL`` monad which contains nested local environments.

### Package Database
``compiler/main/Packages.hs``
See also ``Distribution.Simple``

We can build a package database manually through the ``InstalledPackageInfo``
file, see [Package Guide](https://downloads.haskell.org/~ghc/7.0.4/docs/html/users_guide/packages.html#installed-pkg-info). The ``Distribution.InstalledPackageInfo`` defines the
above as a record data type.

## Cabal Private (CaPri)
Manage a private database of packages, without interfering with the global
or user-local package repositories.
[Cabal Private](https://wiki.haskell.org/Capri)

## Cloning Hackage

[Building and Cloning Hackage](https://github.com/haskell/hackage-server/wiki)

This will probably fail, you also need to create a bunch of keys
using ``hackage-repo-tool`` and copy them over:

    [Hackage-Repo-Tool](https://github.com/haskell/hackage-server/pull/412/files#diff-04c6e90faac2675aa89e2176d2eec7d8R21)

In particular:

    hackage-repo-tool create-keys --keys keys
    cp keys/timestamp/<id>.private datafiles/TUF/timestamp.private
    cp keys/snapshot/<id>.private  datafiles/TUF/snapshot.private
    hackage-repo-tool create-root --keys keys -o datafiles/TUF/root.json
    hackage-repo-tool create-mirrors --keys keys -o datafiles/TUF/mirrors.json

This doesn't work for me.

# Recent Talks about Packages/Modules in Haskelll

[ICFP](https://wiki.haskell.org/HaskellImplementorsWorkshop/2015#.22Look_Ma.2C_No_Signatures.21.22_Separate_modular_development_without_interfaces)


# Parsing Cabal Files

We can use ``Distribution.PackageDescription`` to parse Cabal files:

    [PackageDescription](https://www.haskell.org/cabal/release/cabal-latest/doc/API/Cabal/)

In ``Distribution.PackageDescription.Parse``:

    readPackageDescription
        :: Verbosity -> FilePath -> IO GenericPackageDescription

``Distribution.Verbosity`` has the flags (``normal``,
``verbose``, ``silent``, etc).
