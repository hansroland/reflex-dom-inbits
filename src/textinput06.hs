{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import           Reflex.Dom 
import qualified Data.Text as T

main :: IO ()
main = mainWidget body
         
body :: MonadWidget t m => m ()
body = do
  rec el "h1" $ text "Clear TextInput Widget"
      ti <- textInput $ def & setValue .~ ("" <$ evReset)
      evReset <- button "Reset"
  return ()