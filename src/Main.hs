{-# LANGUAGE DoAndIfThenElse #-}
import Database.HDBC
import Database.HDBC.ODBC
import Data.Foldable
import Control.Applicative
import System.Directory
import Data.Time.LocalTime
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


processOne :: IConnection c
           => c
           -> LocalTime
           -> Either (Int, String, String) DataLine
           -> IO LocalTime
processOne _ t (Left (ln, s, err)) = do
    putStrLn ("Line " ++ show ln)
    print s
    print err
    return t
processOne con t (Right l) = insert t l con


insert :: IConnection c => LocalTime -> DataLine -> c -> IO LocalTime
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

getUsers :: IConnection c => c -> IO ()
getUsers con = do
    --users <- quickQuery con "SELECT DISTINCT(name) FROM text ORDER BY name" []
    users <- quickQuery con "SELECT name, COUNT(*) FROM text GROUP BY name ORDER BY COUNT(*);" []
    print users
    return ()

connect :: IO Connection
connect = do
    let connectionString = "DSN=name32;\
                           \Driver={MySQL ODBC 5.3 ANSI Driver};\
                           \Server=localhost;\
                           \Port=3306;\
                           \Database=testdb;\
                           \User=root;\
                           \Password=password;\
                           \Option=3;"
    conn <- connectODBC connectionString
    return conn

populateDb :: IConnection c => c -> IO ()
populateDb con = do
    logfile <- readConfig
    contents <- lines <$> readFile logfile
    let parsed = parseLine <$> zip [1..] contents
    let time = undefined
    foldlM (processOne con) time parsed
    commit con

main :: IO ()
main = do
    con <- connect

    --populateDb con
    getUsers con
    disconnect con



