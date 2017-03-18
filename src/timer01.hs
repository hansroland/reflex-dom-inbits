{-# LANGUAGE OverloadedStrings #-}
import           Reflex.Dom
import           Data.Time
import           Control.Monad.Trans (liftIO)
import qualified Data.Text as T

main :: IO ()
main = mainWidget bodyElement

bodyElement :: MonadWidget t m =>  m()
bodyElement = do
  el "h2" $ text "A Simple Clock"
  now <- liftIO getCurrentTime
  evTick <- tickLossy 1 now
  let evTime = (T.pack . show . _tickInfo_lastUTC) <$>  evTick
  dynText =<< holdDyn "No ticks yet" evTime