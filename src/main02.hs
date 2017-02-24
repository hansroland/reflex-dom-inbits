{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import Data.Map as Map

main :: IO ()
main = mainWidgetWithHead headElement bodyElement

headElement :: MonadWidget t m => m ()
headElement = do
  el "title" $ text "Main Title"
  styleSheet "css/simple.css"
  where
    styleSheet link = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", link)
      ]) $ return ()

bodyElement :: MonadWidget t m => m ()
bodyElement =  el "div" $ do
     el "h1" $ text "This title should be green"
     return ()