{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid
import           Data.FileEmbed
import           Data.Maybe (isJust, fromJust)
import           Data.Meteo.Swiss

main :: IO ()
main = mainWidgetWithCss css body
   where css = $(embedFile "css/tab.css")

-- | Enumerate our pages
data Page = Page01
   | Page02
   | Page03
  deriving Eq

-- | Create the HTML body element
body :: MonadWidget t m => m ()
body = do
  el "h2" $ text "Swiss Weather Data - All Stations"
  rec dynPage <- foldDyn ($) Page01 $ leftmost [
         const Page01 <$ evOkAll,
         const Page01 <$ leftmost [evEndPg2, evEndPg3],
         const Page02 <$ evOk, 
         const Page03 <$ leftmost [evErr, evErrAll] ]  
      evStart <- getPostBuild
      -- Build and send the request for all stations
      evRspAll <- performRequestAsync $ fmap buildReqAll evStart
      let (evOkAll, evErrAll) = checkXhrRsp evRspAll
      -- Show list of all stations
      evStat <- page01 evOkAll dynPage
       -- Send request for a single station 
      evRsp <- performRequestAsync $ buildReqStat <$> evStat
      let (evOk, evErr) = checkXhrRsp evRsp
      evSmnRec :: (Event t SmnRecord) <- return $  fmapMaybe decodeXhrResponse evOk
      evEndPg2 <- page02 evSmnRec dynPage
      evEndPg3 <- page03 (leftmost [evErr, evErrAll]) dynPage
  return ()

-- | Display the page with a table with all stations
-- Return an event with the 3-letter code of the station
page01 :: MonadWidget t m => Event t XhrResponse -> Dynamic t Page -> m (Event t T.Text)
page01 evRsp dynPage = do 
  let dynAttr = visible <$> dynPage <*> pure Page01
  elDynAttr "div" dynAttr $ do
    -- Convert JSON to list of SmnRecords
    evListRaw :: Event t [SmnRecord] <- return $ fmapMaybe decodeXhrResponse evRsp
    -- We want only SmnRecords with a station name
    let evList = withNames <$> evListRaw
    -- list stations
    el "table" $ do
      dynList :: Dynamic t [SmnRecord] <- holdDyn [] evList
      evRowsDyn <- simpleList dynList displayStationRow
      return $ switchPromptlyDyn $ leftmost <$> evRowsDyn
 
-- | Display the page with the data of a single station
-- Return an event, when the user closes the page
page02 :: MonadWidget t m => Event t SmnRecord -> Dynamic t Page -> m (Event t ())
page02 evSmnRec dynPage = do
  let dynAttr = visible <$> dynPage <*> pure Page02
  elDynAttr "div" dynAttr $ do
    evBack <- button "Back"
    let evSmnStat = fmapMaybe smnStation evSmnRec
    el "div" $
      tabDisplay "tab" "tabact" $ tabMap evSmnRec evSmnStat
    return evBack

-- | Display the error page
-- Return an event, when the user closes the page
page03 :: MonadWidget t m => Event t XhrResponse -> Dynamic t Page -> m (Event t ())
page03 evRsp dynPage = do
  let dynAttr = visible <$> dynPage <*> pure Page03
  elDynAttr "div" dynAttr $ do 
     evBack <- button "Back"
     el "h3" $ text "Error"
     dynText =<< holdDyn "" (_xhrResponse_statusText <$> evRsp)
     return evBack

-- | Split up good and bad response events
checkXhrRsp :: FunctorMaybe f => f XhrResponse -> (f XhrResponse, f XhrResponse)
checkXhrRsp evRsp = (evOk, evRsp)
  where
    evOk = ffilter (\rsp -> _xhrResponse_status rsp == 200) evRsp
    evErr = ffilter (\rsp -> _xhrResponse_status rsp /= 200) evRsp

-- | A button that sends an (Event t T.Text) with the station code as payload
cmdButton :: MonadWidget t m
            => T.Text                       -- ^ Label
            -> Dynamic t SmnRecord
            -> m (Event t T.Text)
cmdButton label staRec = do
    (btn, _) <- el' "button" $ text label
    let dynNam = smnCode <$> staRec
    return $ tagPromptlyDyn dynNam $ domEvent Click btn

-- | Create the HTML element for a single HTML table row
displayStationRow :: MonadWidget t m => Dynamic t SmnRecord -> m (Event t T.Text)
displayStationRow dynRec =  el "tr" $ do
  evRow <- el "td" $ cmdButton "View" dynRec
  el "td" $ dynText $ staName . fromJust . smnStation <$> dynRec
  return evRow

-- | Filter only SmnRecords that really have a name
withNames :: [SmnRecord] -> [SmnRecord]
withNames = filter (isJust . smnStation)

-- | Build the Xhr request to get data for all stations
buildReqAll :: a -> XhrRequest ()
buildReqAll _ = XhrRequest "GET" urlDataAll def

-- | Build the Xhr request to get data for a single station
buildReqStat :: T.Text -> XhrRequest ()
buildReqStat code = XhrRequest "GET" (urlDataStat code) def

-- | Get the station name from a SmnRecord
getStation :: SmnRecord -> T.Text
getStation = getStationName . smnStation
  where 
    getStationName :: Maybe SmnStation -> T.Text
    getStationName (Just staRec) = staName staRec
    getStationName Nothing = "STATION IS NOTHING"

-- | Helper function to create a dynamic attribute map for the visibility of an element
visible :: Page -> Page -> Map.Map T.Text T.Text
visible p1 p2 = "style" =: ("display: " <> choose (p1 == p2) "inline" "none")
  where 
    choose True  t _ = t
    choose False _ f = f

-- ------------------------------------------------------------------------------------------------
-- Code to display the data of a  single station on page02
-- ------------------------------------------------------------------------------------------------

-- | Create a tabbed display
tabMap :: MonadWidget t m => Event t SmnRecord -> Event t SmnStation -> Map.Map Int (T.Text, m ())
tabMap evMeteo evStat = Map.fromList[ (1, ("Station", tabStat evStat)),
            (2, ("MeteoData", tabMeteo evMeteo))]

-- | Create the DOM elements for the Station tab
tabStat :: MonadWidget t m => Event t SmnStation -> m ()
tabStat evStat = do
  dispStatField "Name" staName evStat
  dispStatField "Code" staCode evStat
  dispStatField "Y-Coord" (tShow . staCh1903Y) evStat
  dispStatField "X-Coord" (tShow . staCh1903X) evStat
  dispStatField "Elevation" (tShow . staElevation) evStat
  return ()

-- | Create the DOM elements for the Meteo data tab
tabMeteo :: MonadWidget t m => Event t SmnRecord -> m ()
tabMeteo evMeteo = do 
  dispMeteoField "Date/Time" (tShow . smnDateTime) evMeteo
  dispMeteoField "Temperature" smnTemperature evMeteo
  dispMeteoField "Sunnshine" smnSunshine evMeteo
  dispMeteoField "Precipitation" smnPrecipitation evMeteo
  dispMeteoField "Wind Direction" smnWindDirection evMeteo
  dispMeteoField "Wind Speed" smnWindSpeed evMeteo
  return ()

-- Display a single field from the SmnStation record
dispStatField :: MonadWidget t m => T.Text -> (SmnStation -> T.Text) -> Event t SmnStation -> m ()
dispStatField label rend evStat = do
  el "br" blank
  text $ label <> ": "
  dynText =<< holdDyn "" (fmap rend evStat)
  return ()

-- Display a single field from the SmnRecord record
dispMeteoField :: MonadWidget t m => T.Text -> (SmnRecord -> T.Text) -> Event t SmnRecord -> m ()
dispMeteoField label rend evRec = do
  el "br"blank
  text $ label <> ": "
  dynText =<< holdDyn "" (fmap rend evRec)
  return ()

-- | Small helper function to convert showable values wrapped in Maybe to T.Text. 
-- You should use the test-show library from Hackage!! 
tShow :: Show a => Maybe a -> T.Text
tShow Nothing = ""
tShow (Just x) = (T.pack . show) x