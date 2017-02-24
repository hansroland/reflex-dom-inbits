{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  rec el "h2" $ text "Combining Events with mergeWith and foldDyn"
      dynCount <- foldDyn (+) (0 :: Int)  (mergeWith (+) [1 <$ evIncr, -1 <$ evDecr])
      el "div" $ display dynCount
      evIncr <- button "Increment"
      evDecr <- button "Decrement"
  return ()