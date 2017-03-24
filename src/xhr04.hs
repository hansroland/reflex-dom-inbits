{-# LANGUAGE OverloadedStrings #-}
import           Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Meteo.Swiss

main :: IO ()
main = mainWidget body

body :: MonadWidget t m => m ()
body  = el "div" $ do
  el "h2" $ text "Swiss Weather Data (raw version)"
  text "Choose station: "
  dd <- dropdown "BER" (constDyn stations) def
  evSend <- button "Send"
  -- Build and send the request
  let evCode = tagPromptlyDyn (value dd) evSend
  evRsp <- performRequestAsync $ fmap buildReq evCode
  -- Display the whole response
  el "p" blank
  text "Response Text"
  el "p" blank
  let evResult = (result . _xhrResponse_responseText) <$> evRsp
  dynText =<< holdDyn "" evResult
  return ()

buildReq :: T.Text -> XhrRequest ()
buildReq code = XhrRequest "GET" (urlDataStat code) def

stations :: Map.Map T.Text T.Text
stations = Map.fromList [("BIN", "Binn"), ("BER", "Bern"), ("KLO", "Zurich airport"), ("ZER", "Zermatt"), ("JUN", "Jungfraujoch")]

result :: Show a => Maybe a -> T.Text
result (Just x) = T.pack (show x)
result Nothing = "Response is Nothing"