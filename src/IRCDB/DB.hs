{-# LANGUAGE DoAndIfThenElse #-}
module IRCDB.DB where

import Prelude hiding (foldl, concat, sequence_)
import Control.Applicative
import Database.HDBC
import Database.HDBC.ODBC
import Data.Foldable
import Data.Time.LocalTime
import Text.StringTemplate
import System.Directory
import IRCDB.Parser
import IRCDB.Time
import IRCDB.Renderer

data Action = Repopulate | Generate

configFile :: String
configFile = "config"

readConfig :: IO String
readConfig = do
    exists <- doesFileExist configFile
    if not exists
    then
        error $ "file \"" ++ configFile ++ "\" not found"
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

getUsers :: IConnection c => c -> IO [String]
getUsers con = do
    users <- quickQuery con "SELECT name, COUNT(*) FROM text GROUP BY name ORDER BY COUNT(*);" []
    return $ show <$> users


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

deleteDb :: IConnection c => c -> IO ()
deleteDb con = do
    quickQuery con "DROP TABLE IF EXISTS text;" []
    return ()

createDb :: IConnection c => c -> IO ()
createDb con = do
    let query = "CREATE TABLE text(id BIGINT NOT NULL AUTO_INCREMENT,\
                                  \text VARCHAR(1000),\
                                  \flags VARCHAR(2),\
                                  \name VARCHAR(36),\
                                  \time DATETIME,\
                                  \PRIMARY KEY (id)"
    quickQuery con query []
    return ()

populateDb :: IConnection c => c -> IO ()
populateDb con = do
    logfile <- readConfig
    contents <- lines <$> readFile logfile
    let parsed = parseLine <$> zip [1..] contents
    let time = undefined
    foldlM (processOne con) time parsed
    commit con

repopulateDb :: IConnection c => c -> IO ()
repopulateDb con = do
    deleteDb con
    createDb con
    populateDb con

doAction :: Action -> IO ()
doAction action = do
    con <- connect
    case action of
        Repopulate -> repopulateDb con
        Generate -> do
            template <- readTemplate
            users <- getUsers con
            print $ length users
            sequence_ $ print <$> users
            let fill = (newSTMP template :: StringTemplate String)
            let thing = render $ setManyAttrib (zip (repeat "stuff") users) fill

            writeFile "generated.html" thing
    disconnect con
