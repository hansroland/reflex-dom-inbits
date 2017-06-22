{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid((<>))

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement =  el "div" $ do
  rec
    el "h2" $ text "Some Tracing"
    let group = "g"
    let dynAttrs = styleMap <$> dynColor
    evRad1 <- radioBtn "orange" group Orange dynAttrs
    evRad2 <- radioBtn "green" group Green dynAttrs
    evRad3 <- radioBtn "red" group Red dynAttrs
    let evRadio = (T.pack . show) <$> leftmost [evRad1, evRad2, evRad3]
    let evRadioT = traceEvent ("Clicked rb in group " <> T.unpack group) evRadio
    dynColor <- holdDyn "lightgrey" evRadioT
  return ()

data Color = White | Red | Orange | Green
  deriving (Eq, Ord, Show)

-- | Helper function to create a radio button
radioBtn :: (Eq a, Show a, MonadWidget t m) => T.Text -> T.Text -> a -> Dynamic t ( Map.Map T.Text T.Text)-> m (Event t a)
radioBtn label group rid dynAttrs = do
    el "br" blank 
    ev <- elDynAttr "label" dynAttrs $ do
        (rb1, _) <- elAttr' "input" ("name" =: group <> "type" =: "radio" <> "value" =: T.pack (show rid))  blank
        text label
        return $ domEvent Click rb1
    return $ rid <$ ev

styleMap :: T.Text -> Map.Map T.Text T.Text
styleMap c = "style" =: ("background-color: " <> c)