{-# LANGUAGE DoAndIfThenElse #-}
import Database.HDBC
import Database.HDBC.ODBC
import Control.Applicative
import System.Directory
import System.IO

configFile :: String
configFile = "config"

readConfig :: IO String
readConfig = do
    exists <- not <$> doesFileExist configFile
    if exists
    then
        error "file 'config' not found"
    else do
        config <- lines <$> readFile configFile
        return $ processConfig config
    where processConfig (c:_) = c
          processConfig     _ = error "file 'config' is empty"

main :: IO ()
main = do
    logfile <- readConfig
    contents <- lines <$> readFile logfile
    print contents
    {-let connectionString =  "DSN=name32;Driver={MySQL ODBC 5.3 ANSI Driver};Server=localhost;Port=3306;Database=testdb;User=root;Password=password;Option=3;"
    let ioconn = connectODBC connectionString
    conn <- ioconn
    vals <- quickQuery conn "SELECT version();" []
    print vals-}


