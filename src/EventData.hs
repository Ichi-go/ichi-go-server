module EventData where

import System.IO.Error

import Database.HDBC
import Database.HDBC.Sqlite3

{- | Record used to filter out events when querying. -}
data EventFilter = EventFilter

{- | Record for internal representation of events. -}
data Event = Event { name      :: String
                   , descript  :: String
                   , latitude  :: Double
                   , longitude :: Double
                   , location  :: String
                   } deriving (Show, Eq)

{- | Reconstitute an event from a row aquired from 'quickQuery' -}
reconsEvent [_, sName, sDesc, sLat, sLon, sLoc] =
  Event { name      = (fromSql sName)::String
        , descript  = (fromSql sDesc)::String
        , latitude  = (fromSql sLat )::Double
        , longitude = (fromSql sLon )::Double
        , location  = (fromSql sLoc )::String }

{- | Insert event 'e' into Sqlite3 database 'dbName'. -}
addEvent dbName e = do
  -- Connect
    conn <- connectSqlite3 dbName
    -- Build table
    run conn istat [ toSql $ name e, toSql $ descript e, toSql $ latitude e
                   , toSql $ longitude e, toSql $ location  e]
    -- Commit
    commit conn
    -- Disconnect
    disconnect conn

    where istat = "INSERT INTO events VALUES (NULL, ?, ?, ?, ?, ?);"

{- | Return all events stored in Sqlite3 database 'dbName'. -}
queryEvents dbName = do
  -- Connect to database
  conn <- connectSqlite3 dbName
  -- Get all events
  r <- quickQuery' conn "SELECT * FROM events" []
  return $ map reconsEvent r

{- | Initializes a database to have the needed event table. If the table already
exists this function will do nothing. -}
initDB dbName = do
  -- Connect
  conn <- connectSqlite3 dbName
  -- Build table
  handleSql (\_ -> return (0)) $ run conn "CREATE TABLE events (\
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
