{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import qualified Data.Text as T

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = el "div" $ do
  el "h2" $ text "Checkbox - User friendly"
  cb <- el "label" $ do
    cb1 <- checkbox True def
    text "Click me"
    return cb1
  el "p" blank
  let dynState = checkedState <$> value cb  
  dynText dynState 

checkedState :: Bool -> T.Text
checkedState True = "Checkbox is checked"
checkedState _    = "Checkbox is not checked"