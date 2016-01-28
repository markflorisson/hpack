module HPack.Infer.JSONInterface
(serialize, deserialize)
where

import HPack.System (PkgId(..))
import HPack.Source (Pkg(..), ModulePath(..), showVersion)
import HPack.Infer.IfaceExtract
    (PkgInterface(..), ModInterface(..), Symbol(..), Origin(..))

import GHC.Generics
import Data.Aeson
    (toJSON, fromJSON, Value, ToJSON, FromJSON)
import qualified Data.ByteString as BS

type JSON = Value

-- instance ToJSON PkgInterface
-- instance ToJSON ModInterface
-- instance ToJSON Symbol
-- instance ToJSON Origin
-- instance ToJSON PkgId

-- instance FromJSON PkgInterface
-- instance FromJSON ModInterface
-- instance FromJSON Symbol
-- instance FromJSON Origin
-- instance FromJSON PkgId
-- instance FromJSON ModulePath
-- instance (Generic a, Generic b, FromJSON a, FromJSON b) =>
--          FromJSON (M.Map a b)

serialize :: PkgInterface -> BS.ByteString
serialize = undefined

deserialize :: BS.ByteString -> Maybe PkgInterface
deserialize = undefined
