module EventData where

import Database.HDBC
import Database.HDBC.Sqlite3

data EventFilter = EventFilter
data Event = Event { name      :: String
                   , descript  :: String
                   , latitude  :: Double
                   , longitude :: Double
                   , location  :: String
                   } deriving (Show, Eq)

dbName = "test.db"

reconsEvent [_, sName, sDesc, sLat, sLon, sLoc] =
  Event { name      = (fromSql sName)::String
        , descript  = (fromSql sDesc)::String
        , latitude  = (fromSql sLat )::Double
        , longitude = (fromSql sLon )::Double
        , location  = (fromSql sLoc )::String }

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


initDB dbName = do
  -- Connect
  conn <- connectSqlite3 dbName
  -- Build table
  run conn "CREATE TABLE events (\
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


queryEvents dbName = do
  -- Connect to database
  conn <- connectSqlite3 dbName
  -- Get all events
  r <- quickQuery' conn "SELECT * FROM events" []
  return $ map reconsEvent r
