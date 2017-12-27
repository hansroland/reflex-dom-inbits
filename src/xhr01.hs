{-# LANGUAGE OverloadedStrings #-}
import           Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))

main :: IO ()
main = mainWidget body

body :: MonadWidget t m => m ()
body  = el "div" $ do
  el "h2" $ text "Swiss Meteo Data (raw version)"
  text "Choose station: "
  dd <- dropdown "BER" (constDyn stations) def
  -- Build and send the request
  evStart <- getPostBuild
  let evCode = tagPromptlyDyn (value dd) $ leftmost [ () <$ _dropdown_change dd, evStart]
  evRsp <- performRequestAsync $ buildReq <$> evCode
  -- Display the whole response
  el "h5" $ text "Response Text:"
  let evResult = (fromMaybe "" . _xhrResponse_responseText) <$> evRsp
  dynText =<< holdDyn "" evResult
  return ()

buildReq :: T.Text -> XhrRequest ()
buildReq code = XhrRequest "GET" ("https://opendata.netcetera.com/smn/smn/" <> code) def

stations :: Map.Map T.Text T.Text
stations = Map.fromList [("BIN", "Binn"), ("BER", "Bern"), ("KLO", "Zurich airport"), ("ZER", "Zermatt"), ("JUN", "Jungfraujoch")]