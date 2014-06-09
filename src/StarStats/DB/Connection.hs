{-# LANGUAGE DoAndIfThenElse, NoMonomorphismRestriction, BangPatterns #-}
module StarStats.DB.Connection where

import Database.HDBC
import Database.HDBC.ODBC
import StarStats.DB.Utils
connect :: ServerInfo -> IO Connection
connect (ServerInfo driver dbName) = do
    let connectionString = "DSN=name32;\
                          \ Driver={" ++ driver ++ "};\
                          \ Server=localhost;\
                          \ Port=3306;\
                          \ Database=" ++ dbName ++ ";\
                          \ User=root;\
                          \ Password=password;\
                          \ Option=3;"
    conn <- connectODBC connectionString
    return conn

close :: IConnection c => c -> IO ()
close con = do
    disconnect con
