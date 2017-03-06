{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.Common
import           Reflex.Dom.Contrib.Widgets.ButtonGroup
import qualified Data.Text as T

main :: IO ()
main = mainWidget bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  el "h2" $ text "Radio Buttons from the Contrib Library"
  rec
    rbs :: HtmlWidget t (Maybe Selection) <- 
       radioGroup 
            (constDyn "size") 
            (constDyn [(Small, "small"), (Medium, "Medium"), (Large, "LARGE")])
            WidgetConfig { _widgetConfig_initialValue = Nothing
                         , _widgetConfig_setValue     = never
                         , _widgetConfig_attributes   = constDyn mempty}
    text "Result: "
    display (translate <$> _hwidget_value rbs)
  return ()

-- | A data type for the different choices 
data Selection = Small | Medium | Large
  deriving Eq

-- | Helper function to translate a Selection to an Text value containing a number
translate :: Maybe Selection -> T.Text
translate Nothing = "0"
translate (Just Small) = "10"
translate (Just Medium) = "50"
translate (Just Large) = "800"