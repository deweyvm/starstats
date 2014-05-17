{-# LANGUAGE DoAndIfThenElse, NoMonomorphismRestriction #-}
module IRCDB.DB where

import Prelude hiding (foldl, concat, sequence_)
import Control.Applicative
import Database.HDBC
import Database.HDBC.ODBC
import Data.Foldable
import Data.Time.LocalTime
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
insert t (Message time typ name msg) con = do
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO messages (name, type, text, time) VALUES (?,?,?,?);"
    let sqlName = toSql name
    let sqlType = toSql typ
    let sqlMsg = toSql msg
    let sqlTime = toSql newT
    execute prepared [sqlName, sqlType, sqlMsg, sqlTime]
    return newT
insert t (Nick time old new) con = do
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO nickchanges (oldname, newname, time) VALUES (?,?,?);"
    let sqlOld = toSql old
    let sqlMsg = toSql new
    let sqlTime = toSql newT
    execute prepared [sqlOld, sqlMsg, sqlTime]
    return newT
insert _ (Day date) _ = return date
insert _ (Open date) _ = return date
insert t _ _ = return t

extractPair :: [SqlValue] -> (String, Int)
extractPair (x:y:_) = (fromSql x, fromSql y)
extractPair       _ = undefined

extractMessage :: [SqlValue] -> (String, String)
extractMessage (_:msg:_:name:_) = (fromSql name, fromSql msg)
extractMessage                _ = undefined

getRand :: IConnection c => c -> IO [(String,String)]
getRand con = do
    quickQuery con "SET @max = (SELECT MAX(id) FROM messages); " []
    x <- quickQuery con "SELECT * FROM messages AS v JOIN (SELECT ROUND(RAND() * @max) as v2 FROM messages LIMIT 10) as dummy ON v.id = v2" []
    return $ extractMessage <$> x

getUsers :: IConnection c => c -> IO [(String,Int)]
getUsers con = do
    users <- quickQuery con "SELECT * FROM (SELECT name, COUNT(*) as count FROM messages GROUP BY name ORDER BY count DESC LIMIT 10) AS dummy ORDER BY count" []
    return $ extractPair <$> users

getNicks :: IConnection c => c -> IO [(String,Int)]
getNicks con = do
    nicks <- quickQuery con "SELECT oldname, COUNT(*) as count FROM nickchanges GROUP BY oldname ORDER BY count DESC LIMIT 10" []
    return $ extractPair <$> nicks

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

deleteDbs :: IConnection c => c -> IO ()
deleteDbs con = do
    quickQuery con "DROP TABLE IF EXISTS messages;" []
    quickQuery con "DROP TABLE IF EXISTS statuses;" []
    quickQuery con "DROP TABLE IF EXISTS nickchanges;" []

    return ()

createDbs :: IConnection c => c -> IO ()
createDbs con = do
    let messages = "CREATE TABLE messages(id BIGINT NOT NULL AUTO_INCREMENT,\
                                        \ text VARCHAR(4000),\
                                        \ type INT,\
                                        \ name VARCHAR(36),\
                                        \ time DATETIME,\
                                        \ PRIMARY KEY (id))\
                  \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    let statuses = "CREATE TABLE statuses(id BIGINT NOT NULL AUTO_INCREMENT,\
                                        \ text VARCHAR(4000),\
                                        \ name VARCHAR(36),\
                                        \ time DATETIME,\
                                        \ PRIMARY KEY (id))"
    let nickchanges = "CREATE TABLE nickchanges(id BIGINT NOT NULL AUTO_INCREMENT,\
                                              \ oldname VARCHAR(36),\
                                              \ newname VARCHAR(36),\
                                              \ time DATETIME,\
                                              \ PRIMARY KEY (id))"
    quickQuery con messages []
    quickQuery con statuses []
    quickQuery con nickchanges []
    return ()

populateDbs :: IConnection c => c -> IO ()
populateDbs con = do
    logfile <- readConfig
    contents <- lines <$> readFile logfile
    let parsed = parseLine <$> zip [1..] contents
    let time = undefined
    foldlM (processOne con) time parsed
    commit con

repopulateDb :: IConnection c => c -> IO ()
repopulateDb con = do
    deleteDbs con
    createDbs con
    populateDbs con


generate :: IConnection con => con -> IO ()
generate con = do
    let format (user, num) = user ++ ": " ++ show num
    let lFormat = liftA format
    let headerList s xs = withHeading s $ makeList xs
    rand <- lFormat <$> getRand con
    users <- lFormat <$> getUsers con
    nicks <- lFormat <$> getNicks con
    let rendered = unlines $ (uncurry headerList) <$> [ ("Top Users", users)
                                                      , ("Random Messages", rand)
                                                      , ("Most Changed Nicks", nicks)
                                                      ]
    writeFile "generated.html" $ makeFile rendered
doAction :: Action -> IO ()
doAction action = do
    con <- connect
    case action of
        Repopulate -> repopulateDb con
        Generate -> generate con

    disconnect con
