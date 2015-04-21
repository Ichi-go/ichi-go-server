{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad.IO.Class
    
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import		 Snap.Extras.JSON

import           EventData
import		 Data.Aeson

main :: IO ()
main = do
	initDB "events" --Initializes the DB everytime. We want to change this
	quickHttpServe site

{-| Serves the Ichi-Go web site -}
site :: Snap ()
site =
    ifTop (writeText "Weclome to 一期一会!") <|>
    route [ ("getEvents", eventRequestHandler) ]

          
{-| Returns all requested events as JSON -}          
eventRequestHandler :: Snap ()
eventRequestHandler = do
  events <- liftIO $ queryEvents  "events"
  maybe (writeText "Database error!")
        writeJSON $ Just events
