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

-- | Enumeration to track type of page to display
data Page = PageData | PageError
   deriving Eq

-- | Create the HTML body
body :: MonadWidget t m => m ()
body  = el "div" $ do
  el "h2" $ text "Swiss Weather Data (Tab display)"
  text "Choose station: "
  dd <- dropdown "BER" (constDyn stations) def
  el "p" blank
  -- Build and send the request
  evStart <- getPostBuild
  let evCode = tagPromptlyDyn (value dd) $ leftmost [ () <$ _dropdown_change dd, evStart]
  evRsp <- performRequestAsync $ buildReq <$> evCode
  -- Check on HTML response code and remember state.
  let (evOk, evErr) = checkXhrRsp evRsp
  dynPage <- foldDyn ($) PageData $ leftmost [const PageData <$ evOk, const PageError <$ evErr]
  -- Create the 2 pages
  pageData evOk dynPage
  pageErr evErr dynPage
  return ()

-- | Display the meteo data in a tabbed display
pageData :: MonadWidget t m => Event t XhrResponse -> Dynamic t Page -> m ()
pageData evOk dynPage = do
  evSmnRec :: (Event t SmnRecord) <- return $  fmapMaybe decodeXhrResponse evOk
  let evSmnStat = fmapMaybe smnStation evSmnRec
  let dynAttr = visible <$> dynPage <*> pure PageData
  elDynAttr "div" dynAttr $
    tabDisplay "tab" "tabact" $ tabMap evSmnRec evSmnStat

-- | Display the error page
pageErr :: MonadWidget t m => Event t XhrResponse -> Dynamic t Page -> m ()
pageErr evErr dynPage = do
  let dynAttr = visible <$> dynPage <*> pure PageError
  elDynAttr "div" dynAttr $ do 
     el "h3" $ text "Error"
     dynText =<< holdDyn "" (_xhrResponse_statusText <$> evErr)

-- | Split up good and bad response events
checkXhrRsp :: FunctorMaybe f => f XhrResponse -> (f XhrResponse, f XhrResponse)
checkXhrRsp evRsp = (evOk, evErr)
  where
    evOk = ffilter (\rsp -> _xhrResponse_status rsp == 200) evRsp
    evErr = ffilter (\rsp -> _xhrResponse_status rsp /= 200) evRsp

-- | Helper function to create a dynamic attribute map for the visibility of an element
visible :: Eq p => p -> p -> Map.Map T.Text T.Text
visible p1 p2 = "style" =: ("display: " <> choose (p1 == p2) "inline" "none")
  where 
    choose True  t _ = t
    choose False _ f = f

buildReq :: T.Text -> XhrRequest ()
buildReq code = XhrRequest "GET" (urlDataStat code) def

stations :: Map.Map T.Text T.Text
stations = Map.fromList [("BIN", "Binn"), ("BER", "Bern"), ("KLO", "Zurich airport"), ("ZER", "Zermatt"), ("JUN", "Jungfraujoch")]

-- | Create a tabbed display
tabMap :: MonadWidget t m => Event t SmnRecord -> Event t SmnStation -> Map.Map Int (T.Text, m ())
tabMap evMeteo evStat = Map.fromList[ (1, ("Station", tabStat evStat)),
            (2, ("MeteoData", tabMeteo evMeteo))]

-- | Create the DOM elements for the Station tab
tabStat :: MonadWidget t m => Event t SmnStation -> m ()
tabStat evStat = do 
  dispStatField "Code" staCode evStat
  dispStatField "Name" staName evStat
  dispStatField "Y-Coord" (tShow . staCh1903Y) evStat
  dispStatField "X-Coord" (tShow . staCh1903X) evStat
  dispStatField "Elevation" (tShow . staElevation) evStat
  return ()

-- | Create the DOM elements for the Meteo data tab
tabMeteo :: MonadWidget t m => Event t SmnRecord -> m ()
tabMeteo evMeteo = do 
  dispMeteoField "Date/Time" (tShow . smnDateTime) evMeteo
  dispMeteoField "Temperature" smnTemperature evMeteo
  dispMeteoField "Sunshine" smnSunshine evMeteo
  dispMeteoField "Precipitation" smnPrecipitation evMeteo
  dispMeteoField "Wind Direction" smnWindDirection evMeteo
  dispMeteoField "Wind Speed" smnWindSpeed evMeteo
  return ()

-- | Display a single field from the SmnStation record
dispStatField :: MonadWidget t m => T.Text -> (SmnStation -> T.Text) -> Event t SmnStation -> m ()
dispStatField label rend evStat = do
  el "br" blank
  text $ label <> ": "
  dynText =<< holdDyn "" (fmap rend evStat)
  return ()

-- | Display a single field from the SmnRecord record
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