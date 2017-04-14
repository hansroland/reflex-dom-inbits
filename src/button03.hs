{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Reflex.Dom
import Data.FileEmbed
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Map as M
import Data.Monoid

main :: IO ()
main = mainWidgetWithCss css bodyElement
   where css = $(embedFile "css/button02.css")

bodyElement :: MonadWidget t m => m ()
bodyElement = el "div" $ do
  btn <- fmap fst . el' "button" $ text "light"
  color <- holdDyn White $ touchMouse Red White btn
  light color

-- | A data type for our colors
data Color = White | Red deriving Show

touchMouse
   :: (Reflex t, HasDomEvent t a1) => a -> a -> a -> Event t a
touchMouse x y w = leftmost 
        [   x <$ domEvent Mousedown w
        ,   y <$ domEvent Mouseup w
        ]
        -- ,   x <$ domEvent Touchstart w
        -- ,   y <$ domEvent Touchend w]


light :: (Show a, MonadWidget t m) => Dynamic t a -> m ()
light color = do
    attrs <- mapDyn (\c -> M.fromList [("style","background-color:" <> T.pack (show c)),("class","light")]) color
    elDynAttr "div" attrs $ return ()



