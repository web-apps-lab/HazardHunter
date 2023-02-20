module HazardHunter.Htmx where

import Butler
import Butler.Prelude hiding (decode)
import Data.Aeson as Aeson (Result (Error, Success), Value, decode, fromJSON, (.:))
import Data.Aeson.Key as Aeson (Key, toText)
import Data.Aeson.KeyMap as Aeson (filterWithKey, fromList, map, toMapText)
import qualified Data.Aeson.Text as Aeson
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.Map
import Data.Text.Lazy (toStrict)
import qualified Network.WebSockets as WS
import Prelude

-- xStaticFiles :: [XStatic.XStaticFile]
-- xStaticFiles = [XStatic.htmx, XStatic.htmxExtWS]

-- | Attributes definition for Lucid
hxWS, hxSwapOOB, wsConnect, wsSend, hxTrigger, hxTarget, hxVals :: Text -> Attribute
hxWS = makeAttribute "hx-ws"
hxSwapOOB = makeAttribute "hx-swap-oob"
wsConnect = makeAttribute "ws-connect"
wsSend = makeAttribute "ws-send"
hxTrigger = makeAttribute "hx-trigger"
hxTarget = makeAttribute "hx-target"
hxVals = makeAttribute "hx-vals"

hxExtWS :: Attribute
hxExtWS = makeAttribute "hx-ext" "ws"

data WSwapStrategy = InnerHTML | BeforeBegin

swapToText :: WSwapStrategy -> Text
swapToText sw = case sw of
  InnerHTML -> "innerHTML"
  BeforeBegin -> "beforebegin"

type WidgetId = Text

type TriggerId = Text

type Trigger = Text

decodeWSEvent :: WS.DataMessage -> Maybe WSEvent
decodeWSEvent (WS.Text dm _) = decode dm
decodeWSEvent _ = Nothing

data WSEvent = WSEvent
  { wseTId :: TriggerId,
    wseHeaders :: Map Text (Maybe Text),
    wseData :: Map Text (Maybe Text)
  }
  deriving (Show)

instance FromJSON WSEvent where
  parseJSON (Object v) = do
    headers <- v .: "HEADERS"
    triggerId <- extractTriggerId headers
    pure $ WSEvent triggerId headers extraKeys
    where
      extraKeys = do
        let filtered =
              Aeson.filterWithKey
                (\k _ -> not $ Aeson.toText k == "HEADERS" || Aeson.toText k == "widgetId")
                v
        Aeson.toMapText $
          Aeson.map
            ( \nv -> case nv of
                (String s) -> Just s
                _ -> Nothing
            )
            filtered
      extractTriggerId headers = case Data.Map.lookup "HX-Trigger" headers of
        Just (Just tid) -> pure tid
        _ -> fail "Unable to extract HX-Trigger from HEADERS"
  parseJSON other = fail $ "Unable to parse WSEvent: " <> show other

mkHxVals :: [(Key, Text)] -> Attribute
mkHxVals vals =
  hxVals
    . toStrict
    . Aeson.encodeToLazyText
    $ Aeson.fromList vals

getFromJSON :: FromJSON a => Aeson.Value -> Maybe a
getFromJSON jsonV = case Aeson.fromJSON jsonV of
  Aeson.Success v -> Just v
  Aeson.Error _ -> Nothing
