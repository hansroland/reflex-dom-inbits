{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom

main :: IO()
main = mainWidget $ do
  el "h1" $ text "Welcome to Reflex-Dom"
  el "div" $ do
    el "p" $ text "Reflex-Dom is:"
    el "ul" $ do
      el "li" $ text "Fun"
      el "li" $ text "Not difficult"
      el "li" $ text "Efficient"