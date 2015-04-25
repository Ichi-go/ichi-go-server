{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module EventData where

import Control.Applicative
import GHC.Generics
import System.IO.Error
import System.IO.Error

import Data.Aeson
import Database.HDBC
import Database.HDBC.Sqlite3


{- | Record used to filter out events when querying. -}
data EventFilter = EventFilter

{- | Record for internal representation of events. -}
data Event = Event { name         :: String
                   , description  :: String
                   , latitude     :: Double
                   , longitude    :: Double
                   , location     :: String
                   } deriving (Show, Eq, Generic)

{- | Define the JSON conversion for the Event type. -}
instance ToJSON Event
instance FromJSON Event
           
{- | Reconstitute an event from a row aquired from 'quickQuery' -}
reconsEvent [_, sName, sDesc, sLat, sLon, sLoc] =
  Event { name         = (fromSql sName)::String
        , description  = (fromSql sDesc)::String
        , latitude     = (fromSql sLat )::Double
        , longitude    = (fromSql sLon )::Double
        , location     = (fromSql sLoc )::String }

{- | Insert event 'e' into Sqlite3 database 'dbName'. -}
addEvent dbName e = do
  -- Connect
  conn <- connectSqlite3 dbName
  -- Build table
  let istat = "INSERT INTO events VALUES (NULL, ?, ?, ?, ?, ?);"
  run conn istat [ toSql $ name e, toSql $ description e, toSql $ latitude e
                 , toSql $ longitude e, toSql $ location  e]
  -- Commit
  commit conn
  -- Disconnect
  disconnect conn

{- | Return all events stored in Sqlite3 database 'dbName'. -}
queryEvents dbName = do
  -- Connect to database
  conn <- connectSqlite3 dbName
  -- Get all events
  r <- quickQuery' conn "SELECT * FROM events" []
  -- Disconnect
  disconnect conn
  return $ map reconsEvent r

{- | Initializes a database to have the needed event table. If the table already
exists this function will do nothing.-}
initDB dbName = do
  -- Connect
  conn <- connectSqlite3 dbName
  -- Build table
  handleSql (\_ -> return (0)) $ run conn
    "CREATE TABLE events (\
    \ id INTEGER PRIMARY KEY,\
    \ name VARCHAR(127),\
    \ descript TEXT,\
    \ lat REAL,\
    \ lon REAL,\
    \ location VARCHAR(255)\
    \ );" []
  -- Commit
  commit conn
  -- Disconnect
  disconnect conn
