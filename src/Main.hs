{-# LANGUAGE DoAndIfThenElse #-}
import Database.HDBC
import Database.HDBC.ODBC
import Data.Foldable
import Control.Applicative
import System.Directory
import Data.Time.Clock
import IRCDB.Parser
import IRCDB.Time


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


processOne :: IConnection con
           => con
           -> UTCTime
           -> Either (Int, String, String) DataLine
           -> IO UTCTime
processOne _ t (Left (ln, s, err)) = do
    putStrLn ("Line " ++ show ln)
    print s
    print err
    return t
processOne con t (Right l) = insert t l con


insert :: IConnection con => UTCTime -> DataLine -> con -> IO UTCTime
insert t (Message time op name msg) con = do
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO text (name, flags, text, time) VALUES (?,?,?,?);"
    let sqlName = toSql name
    let sqlOp = toSql op
    let sqlMsg = toSql msg
    let sqlTime = toSql newT
    execute prepared [sqlName, sqlOp, sqlMsg, sqlTime]
    return newT
insert _ (Day date) _ = return date
insert _ (Open date) _ = return date
insert t _ _ = return t


main :: IO ()
main = do
    logfile <- readConfig
    contents <- lines <$> readFile logfile
    let parsed = parseLine <$> zip [1..] contents
    let connectionString = "DSN=name32;\
                           \Driver={MySQL ODBC 5.3 ANSI Driver};\
                           \Server=localhost;\
                           \Port=3306;\
                           \Database=testdb;\
                           \User=root;\
                           \Password=password;\
                           \Option=3;"
    conn <- connectODBC connectionString
    time <- getCurrentTime
    foldlM (processOne conn) time parsed
    commit conn
    disconnect conn


