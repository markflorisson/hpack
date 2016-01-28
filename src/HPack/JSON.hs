module HPack.JSON
(JSON(..), Value(..), ToJSON(..), FromJSON(..), T.Text
, encode, decode, typeMismatch, textToString
, array, tuple
) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Vector as V

import Data.Aeson
import Data.Aeson.Types

type JSON = Value

textToString :: T.Text -> String
textToString = T.unpack

instance (ToJSON a, ToJSON b) => ToJSON (M.Map a b) where
    toJSON = array . map toJSON . M.assocs

instance (Ord a, FromJSON a, FromJSON b) => FromJSON (M.Map a b) where
    parseJSON (Array vals) = do
        pairs <- mapM parseJSON (V.toList vals)
        return (M.fromList pairs)
    parseJSON val = typeMismatch "M.Map" val

array :: [JSON] -> JSON
array = Array . V.fromList

tuple :: (JSON, JSON) -> JSON
tuple (x, y) = array [x, y]
