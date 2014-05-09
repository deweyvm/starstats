{-# LANGUAGE DoAndIfThenElse #-}
--import Database.HDBC
--import Database.HDBC.ODBC
import Control.Applicative
import Control.Arrow
import System.Directory
import IRCDB.Parser

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


catEithers :: [Either a b] -> ([a], [b])
catEithers [] = (,) [] []
catEithers (Right x : xs) = second (x:) $ catEithers xs
catEithers (Left  x : xs) = first (x:) $ catEithers xs


main :: IO ()
main = do
    logfile <- readConfig
    contents <- readFile logfile
    putStrLn contents
    let thing = parseFile contents
    print thing
    {-let connectionString =  "DSN=name32;Driver={MySQL ODBC 5.3 ANSI Driver};Server=localhost;Port=3306;Database=testdb;User=root;Password=password;Option=3;"
    let ioconn = connectODBC connectionString
    conn <- ioconn
    vals <- quickQuery conn "SELECT version();" []
    print vals-}


