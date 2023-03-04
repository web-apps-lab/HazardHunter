module HazardHunter.Htmx where

import Butler
import Butler.Prelude hiding (decode)
import Data.Aeson.Key as Aeson (Key)
import Data.Aeson.KeyMap as Aeson (fromList)
import qualified Data.Aeson.Text as Aeson
import Data.Text.Lazy (toStrict)
import Prelude

hxVals :: Text -> Attribute
hxVals = makeAttribute "hx-vals"

mkHxVals :: [(Key, Text)] -> Attribute
mkHxVals vals =
  hxVals
    . toStrict
    . Aeson.encodeToLazyText
    $ Aeson.fromList vals
