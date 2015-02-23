{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

main :: IO ()
main = quickHttpServe site


site :: Snap ()
site =
    ifTop (writeText "Weclome to 一期一会!") <|>
    route [ ("getEvents", eventRequestHandler)
          , ("getEvents/:eventparam", eventRequestHandler) ]

          
eventRequestHandler :: Snap ()
eventRequestHandler = do
  param <- getParam "eventparam"
  maybe (writeBS "Sorry, I can't find any events for you")
        writeBS param
