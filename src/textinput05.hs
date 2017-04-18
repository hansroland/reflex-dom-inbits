{-# LANGUAGE OverloadedStrings #-}
import           Reflex.Dom 
import qualified Data.Text as T

main :: IO ()
main = mainWidget body
         
body :: MonadWidget t m => m ()
body = do
  el "h1" $ text "Write into TextInput Widget"
  t1 <- textInput def
  evCopy <- button ">>>"
  let evText = tagPromptlyDyn (value t1) evCopy
  t2 <- textInput $ def & setValue .~ evText
  return ()