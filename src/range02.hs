{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import qualified Data.Text as T

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = do
    el "h2" $ text "Range Input"
    rg <- rangeInput $ def & attributes .~ constDyn ("min" =: "-100")
    el "p" blank
    display $ _rangeInput_value rg
    return ()