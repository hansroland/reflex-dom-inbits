{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import           Reflex.Dom
import qualified Data.Text as T 
import qualified Data.Map as Map
import           Data.Monoid ((<>))

main :: IO ()
main = mainWidget $ do
  rec
    dynBool <- toggle False evClick
    let dynAttrs = attrs <$> dynBool
    elDynAttr "h1" dynAttrs $ text "Changing color"
    evClick <- button "Change Color"
  return ()

attrs :: Bool -> Map.Map T.Text T.Text
attrs b = "style" =: ("color: " <> color b)
  where 
    color True = "red"
    color _    = "green"
