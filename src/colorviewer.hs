{-# LANGUAGE OverloadedStrings #-}
import           Reflex.Dom
import           Data.Map
import qualified Data.Text as T

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = do
    el "h2" $ text "Several Text Inputs: RGB Viewer"
    el "div" $ text "Enter RGB component values as numbers between 0 and 255"
    dfsRed <- labledBox "Red: "
    dfsGreen <- labledBox "Green: "
    dfsBlue <- labledBox "Blue: "
    textArea $ 
        def & attributes .~ (styleMap <$> value dfsRed <*> value dfsGreen <*> value dfsBlue) 
    return ()

labledBox :: MonadWidget t m => T.Text -> m (TextInput t)
labledBox lbl = el "div" $ do
    text lbl
    textInput $ def & textInputConfig_inputType .~ "number"
                    & textInputConfig_initialValue .~ "0"
    
styleMap :: T.Text -> T.Text -> T.Text -> Map T.Text T.Text
styleMap r g b = "style" =: mconcat ["background-color: rgb(", r, ", ", g, ", ", b, ")"]