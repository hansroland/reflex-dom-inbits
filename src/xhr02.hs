{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
import           Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.FileEmbed
import           Data.Meteo.Swiss

main :: IO ()
main = mainWidgetWithCss css body
   where css = $(embedFile "css/tab.css")

body :: MonadWidget t m => m ()
body  = el "div" $ do
  el "h2" $ text "Swiss Weather Data (Tab display)"
  text "Choose station: "
  dd <- dropdown "BER" (constDyn stations) def
  -- Build and send the request
  evStart <- getPostBuild
  let evCode = tagPromptlyDyn (value dd) $ leftmost [ () <$ _dropdown_change dd, evStart]
  evRsp <- performRequestAsync $ buildReq <$> evCode
  -- Show error msg
  dynError <- holdDyn False $ (\rsp -> _xhrResponse_status rsp /= 200)  <$> evRsp
  let dynAttrVisError = visible <$> dynError
  elDynAttr "div" dynAttrVisError $ do 
     el "h3" $ text "Error"
     dynText =<< (holdDyn "" $ _xhrResponse_statusText <$> evRsp)
  -- Show a tabbed tabDisplay
  evSmnRec :: (Event t SmnRecord) <- return $  fmapMaybe decodeXhrResponse evRsp
  let evSmnStat = fmapMaybe smnStation evSmnRec
  let dynAttrVisData = (visible . not) <$> dynError
  elDynAttr "div" dynAttrVisData $ do
    el "p" blank
    tabDisplay "tab" "tabact" $ tabMap evSmnRec evSmnStat
  return ()

buildReq :: T.Text -> XhrRequest ()
buildReq code = XhrRequest "GET" ("http://opendata.netcetera.com:80/smn/smn/" <> code) def

stations :: Map.Map T.Text T.Text
stations = Map.fromList [("BIN", "Binn"), ("BER", "Bern"), ("KLO", "Zurich airport"), ("ZER", "Zermatt"), ("JUN", "Jungfraujoch")]

-- | Create a tabbed display
tabMap :: MonadWidget t m => Event t SmnRecord -> Event t SmnStation -> Map.Map Int (T.Text, m ())
tabMap evRec evStat = Map.fromList[ (1, ("Station", tabStat evStat)),
            (2, ("MeteoData", tabRec evRec))]

-- | Create the DOM elements for the Station tab
tabStat :: MonadWidget t m => Event t SmnStation -> m ()
tabStat evStat = do 
  el "h3" $ text "Station"
  dispStatField "Code" staCode evStat
  dispStatField "Name" staName evStat
  dispStatField "Y-Coord" (tShow . staCh1903Y) evStat
  dispStatField "X-Coord" (tShow . staCh1903X) evStat
  dispStatField "Elevation" (tShow . staElevation) evStat
  return ()

-- | Create the DOM elements for the Data tab
tabRec :: MonadWidget t m => Event t SmnRecord -> m ()
tabRec evRec = do 
  el "h3" $ text "Meteo Data"
  dispRecField "Date/Time" (tShow . smnDateTime) evRec
  dispRecField "Temperature" smnTemperature evRec
  dispRecField "Sunnshine" smnSunshine evRec
  dispRecField "Precipitation" smnPrecipitation evRec
  dispRecField "Wind Direction" smnWindDirection evRec
  dispRecField "Wind Speed" smnWindSpeed evRec
  return ()

-- Display a single field from the SmnStation record
dispStatField :: MonadWidget t m => T.Text -> (SmnStation -> T.Text) -> Event t SmnStation -> m ()
dispStatField label rend evStat = do
  el "br" blank
  text $ label <> ": "
  dynText =<< holdDyn "" (fmap rend evStat)
  return ()

-- Display a single field from the SmnRecord record
dispRecField :: MonadWidget t m => T.Text -> (SmnRecord -> T.Text) -> Event t SmnRecord -> m ()
dispRecField label rend evRec = do
  el "br"blank
  text $ label <> ": "
  dynText =<< holdDyn "" (fmap rend evRec)
  return ()

-- | Small helper function to convert showable values to T.Text. 
-- You should use the test-show library from Hackage!! 
tShow :: Show a => Maybe a -> T.Text
tShow Nothing = ""
tShow (Just x) = (T.pack . show) x

-- | Helper function to create a dynamic attribute map for the visibility of an element
visible :: Bool -> Map.Map T.Text T.Text
visible b = "style" =: ("display: " <> choose b "inline" "none")
-- visible b = "style" =: ("visibility: " <> choose b "visible" "invisible")
  where 
    choose True  t _ = t
    choose False _ f = f