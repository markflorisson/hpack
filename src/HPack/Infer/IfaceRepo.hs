module HPack.Infer.IfaceRepo

where

import HPack.Source (Pkg)
import HPack.Infer.IfaceExtract (PkgInterface)

data IfaceRepo
    = IfaceRepo { path :: FilePath }

data IfaceRepoError
    = IfaceNotFound Pkg
    | IfaceLoadError IOError
    | IfaceParseError String

openIfaceRepo :: FilePath -> IO IfaceRepo
openIfaceRepo = undefined

getPkgIface :: IfaceRepo -> Pkg -> IO (Either IfaceRepoError PkgInterface)
getPkgIface = undefined
