{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  rec el "h2" $ text "Combining Events with mergeWith"
      counts <- foldDyn (+) (0 :: Int) $ leftmost [1 <$ incr, -1 <$ decr]
      el "div" $ display counts
      incr <- button "Increment"
      decr <- button "Decrement"
  return ()