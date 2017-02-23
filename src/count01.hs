{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom

main :: IO ()
main = mainWidget $ display =<< count =<< button "ClickMe"