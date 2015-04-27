{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.ByteString.Char8
import           Data.Maybe

import		 Data.Aeson
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import		 Snap.Extras.JSON

import           EventData

defaultDBName = "events"
        
main :: IO ()
main = do
  Prelude.putStr "Weclome to 一期一会!"
  Prelude.putStr "Initializing Database..."
  
  initDB defaultDBName --Initializes the DB everytime. We want to change this
  quickHttpServe site

{-| Serves the Ichi-Go web site -}
site :: Snap ()
site =
  ifTop (writeText "Weclome to 一期一会!") <|>
  route [ ("getEvents", eventRequestHandler),
          ("addEvent", addEventHandler) 
        ]

          
{-| Returns all requested events as JSON -}          
eventRequestHandler :: Snap ()
eventRequestHandler = do
  bfilter <- filter
  events  <- liftIO $ queryEvents defaultDBName bfilter
  maybe (writeText "Database error!")
        writeJSON $ Just events
  where
    filter = do
      rawLat <- getPostParam "lat"
      rawLon <- getPostParam "lon"
      return $ makeEventFilter rawLat rawLon

{-| Returns a Maybe EventFilter from the QueryParams used in
    eventRequestHandler. Right now has a magical number for radius. -}
makeEventFilter :: Maybe ByteString -> Maybe ByteString -> Maybe EventFilter
makeEventFilter rawLat rawLon = do
  lat <- rawLat
  lon <- rawLon
  return $  EventFilter (read $ unpack lat) (read $ unpack lon) 0.3


{-| Searches the body for an event in JSON and adds it to the DB -}
addEventHandler :: Snap ()
addEventHandler = do
  event <- reqJSON :: Snap Event
  eventId <- liftIO $ addEvent defaultDBName event
  writeText eventId
