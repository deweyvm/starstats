{-# LANGUAGE DoAndIfThenElse, NoMonomorphismRestriction, BangPatterns #-}
module StarStats.DB.Connection where

import Database.HDBC
import Database.HDBC.ODBC
import StarStats.DB.Utils
import qualified Data.Text as T

sanitize :: String -> String
sanitize s =
    let p = T.pack s in
    T.unpack $ T.filter (\c -> (not . elem c) "{};\"=") p

connect :: ServerInfo -> IO Connection
connect (ServerInfo driver dbName) = do
    let sDriver = sanitize driver
    let sDbName = sanitize dbName
    let connectionString = "DSN=starstats;\
                          \ Driver={" ++ sDriver ++ "};\
                          \ Server=localhost;\
                          \ Port=3306;\
                          \ Socket=/var/run/mysqld/mysqld.sock;\
                          \ Database=" ++ sDbName ++ ";\
                          \ User=root;\
                          \ Password=password;\
                          \ Option=3;"
    conn <- connectODBC connectionString
    return conn

close :: IConnection c => c -> IO ()
close con = do
    disconnect con
