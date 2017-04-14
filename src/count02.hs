{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO ()
main = mainWidget $ do
  rec el "h2" $ text "Combining Dynamics"
      ups <- count evIncr   
      downs <- count evDecr
      el "div" $ display $ (-) <$> ups <*> downs
      evIncr <- button "Increment"
      evDecr <- button "Decrement"
  return ()