{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Reflex.Dom
import qualified Data.Text as T
import           Data.Monoid

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = el "div" $ do
  el "h2" $ text "Button enabled / disabled"
  cb <- el "label" $ do
    cb1 <- checkbox True def
    text "Enable or Disable the button"
    return cb1
  el "p" blank
  counter :: Dynamic t Int <- count =<< disaButton (_checkbox_value cb) "Click me"
  el "p" blank
  display counter

-- | A button that can be enabled and disabled
disaButton :: MonadWidget t m
            => Dynamic t Bool -- ^ enable or disable button
            -> T.Text         -- ^ Label
            -> m (Event t ())
disaButton enabled label = do
    let attrs = ffor enabled $ \e -> monoidGuard (not e) $ "disabled" =: "disabled"
    (btn, _) <- elDynAttr' "button" attrs $ text label
    pure $ domEvent Click btn

-- | A little helper function for monoid data types: 
-- If the boolean is True, return the first parameter, else return the null element of the monoid
monoidGuard :: Monoid a => Bool -> a -> a
monoidGuard p a = if p then a else mempty

