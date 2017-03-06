{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

import Data.Map
import qualified Data.Text as T
import Data.Monoid ((<>))

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = do
    el "h2" $ text "Range Input"
    rg <- rangeInput $ def & attributes .~ constDyn 
        ("min" =: "-100" <> "max" =: "100" <> "value" =: "0" <> "step" =: "10" <> "list" =: "powers" )
    elAttr "datalist" ("id" =: "powers") $ do
       elAttr "option" ("value" =: "0") blank
       elAttr "option" ("value" =: "-30") blank
       elAttr "option" ("value" =: "50") blank
    el "p" blank
    display $ _rangeInput_value rg
    return ()