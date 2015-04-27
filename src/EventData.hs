{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module EventData where

import Control.Applicative
import GHC.Generics
import System.IO.Error

import Data.Aeson
import Database.HDBC
import Database.HDBC.Sqlite3


{- | Record used to filter out events when querying. -}
data EventFilter = EventFilter { flat   :: Double
                               , flon   :: Double
                               , radius :: Double
                               } deriving (Show, Eq)

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
  conn <- connectSqlite3 dbName
  let istat = "INSERT INTO events VALUES (NULL, ?, ?, ?, ?, ?);"
  run conn istat [ toSql $ name e, toSql $ description e, toSql $ latitude e
                 , toSql $ longitude e, toSql $ location  e ]
  r <- quickQuery' conn "SELECT last_insert_rowid() as last_id" []
  commit conn
  disconnect conn
  (return . fromSql . head . head) r

{- | Return all events stored in Sqlite3 database 'dbName'. -}
queryEvents dbName filter = do
  conn <- connectSqlite3 dbName
  r    <- quickQuery' conn qstring qdata
  disconnect conn
  return $ map reconsEvent r
  where
    qdata = maybe [] (\f -> [ toSql $ (flat f) - (radius f)
                            , toSql $ (flat f) + (radius f)
                            , toSql $ (flon f) - (radius f)
                            , toSql $ (flon f) + (radius f) ]) filter
    qstring = maybe "SELECT * FROM events" (\_ -> "SELECT * FROM events \
                                                  \ WHERE lat BETWEEN ? AND ?\
                                                  \ AND   lon BETWEEN ? AND ?") filter
                                         

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
  commit conn
  disconnect conn
