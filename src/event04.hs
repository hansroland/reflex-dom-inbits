{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO ()
main = mainWidget bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
   el "h2" $ text "Using foldDyn with function application"
   rec dynNum <- foldDyn ($) (0 :: Int) $ leftmost [(+ 1) <$ evIncr, (+ (-1)) <$ evDecr, const 0 <$ evReset]  
       el "div" $ display dynNum
       evIncr <- button "Increment"
       evDecr <- button "Decrement"
       evReset <- button "Reset"
   return ()