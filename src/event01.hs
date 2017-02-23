{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom

main :: IO () 
main = mainWidget bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  rec el "h2" $ text "Counter as a fold"
      numbs <- foldDyn (+) (0 :: Int)  (1 <$ evIncr)
      el "div" $ display numbs
      evIncr <- button "Increment"
  return ()