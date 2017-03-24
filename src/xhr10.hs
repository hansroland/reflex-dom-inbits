{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Monoid
import Data.Maybe (isJust, fromJust)

import Data.Meteo.Swiss

main :: IO ()
main = mainWidget body

body :: MonadWidget t m => m ()
body  = el "div" $ do
  el "h2" $ text "Swiss Weather Data (raw version)"
  text "List stations"
  evSend <- button "Send"
  -- Build and send the request
  evRsp <- performRequestAsync $ fmap buildReq evSend
  -- Get the response
  let evListRaw = fmapMaybe decodeXhrResponse evRsp

  let evList = withNames <$> evListRaw

  let evCount = fmap numbStats evList

  let evTrac = traceEvent "Count" evCount

  dynText =<< holdDyn "" (fmap (T.pack . show ) evTrac)

  el "p" blank

  dynList :: Dynamic t [SmnRecord] <- holdDyn [] evList

  

  simpleList dynList displayStationName


 {-
  -- Display the whole response
  el "p" blank
  text "Response Text"
  el "p" blank
  let evResult = (result . _xhrResponse_responseText) <$> evRsp
  dynText =<< holdDyn "" evResult
 -}

  return ()

 
withNames :: [SmnRecord] -> [SmnRecord]
withNames = filter (isJust . smnStation)

asMap :: [SmnRecord] -> Map.Map T.Text SmnRecord
asMap rs = Map.fromList $ map (\r  -> (,) ((staCode . fromJust . smnStation) r) r) rs

displayStationName :: MonadWidget t m => Dynamic t SmnRecord -> m ()
displayStationName dynSmn = do
  el "p" blank
  dynText $ getStation <$> dynSmn
  return ()

buildReq :: a -> XhrRequest ()
buildReq _ = XhrRequest "GET" "http://opendata.netcetera.com:80/smn/smn/" def

-- stations :: Map.Map T.Text T.Text
-- stations = Map.fromList [("BIN", "Binn"), ("BER", "Bern"), ("KLO", "Zurich airport"), ("ZER", "Zermatt"), ("JUN", "Jungfraujoch")]

result :: Show a => Maybe a -> T.Text
result (Just x) = "Received: " <> T.pack (show x)
result Nothing = "Response is Nothing"


getStation :: SmnRecord -> T.Text
getStation = getStationName . smnStation


getStationName :: Maybe SmnStation -> T.Text
getStationName (Just starec) = staName starec
getStationName Nothing = "STATION IS NOTHING"

numbStats :: [SmnRecord] -> Int
numbStats = length