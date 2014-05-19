{-# LANGUAGE DoAndIfThenElse, NoMonomorphismRestriction #-}
module IRCDB.DB where

import Prelude hiding (foldl, concat, sequence_)
import Control.Applicative
import Database.HDBC
import Database.HDBC.ODBC
import Data.Foldable
import Data.Function
import Data.Time.LocalTime
import qualified Data.MultiMap as MMap
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
    prepared <- prepare con "INSERT INTO messages (name, type, text, time)\
                           \ VALUES (?,?,?,?);"
    let sqlName = toSql name
    let sqlType = toSql typ
    let sqlMsg = toSql msg
    let sqlTime = toSql newT
    execute prepared [sqlName, sqlType, sqlMsg, sqlTime]
    return newT
insert t (Nick time old new) con = do
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO nickchanges (oldname, newname, time)\
                           \ VALUES (?,?,?);"
    let sqlOld = toSql old
    let sqlMsg = toSql new
    let sqlTime = toSql newT
    execute prepared [sqlOld, sqlMsg, sqlTime]
    return newT
insert t (Kick time kickee kicker reason) con = do
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO kicks (kicker, kickee, reason, time)\
                           \ VALUES (?,?,?, ?);"
    let sqlKicker = toSql kicker
    let sqlKickee = toSql kickee
    let sqlReason = toSql reason
    let sqlTime = toSql newT
    execute prepared [sqlKicker, sqlKickee, sqlReason, sqlTime]
    return newT
insert t (Topic time setter topic) con = do
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO topics (name, topic, time)\
                           \ VALUES (?,?,?);"
    let sqlName = toSql setter
    let sqlTopic = toSql topic
    let sqlTime = toSql newT
    execute prepared [sqlName, sqlTopic, sqlTime]
    return newT
insert _ (Day date) _ = return date
insert _ (Open date) _ = return date
insert t _ _ = return t

extractPair :: [SqlValue] -> (String, Int)
extractPair (x:y:_) = (fromSql x, fromSql y)
extractPair       _ = undefined

extractTopic :: [SqlValue] -> (String, String)
extractTopic (_:name:msg:_) = (fromSql name, fromSql msg)
extractTopic                _ = undefined

extractMessage :: [SqlValue] -> (String, String)
extractMessage (_:msg:_:name:_) = (fromSql name, fromSql msg)
extractMessage                _ = undefined


type Extract a = [SqlValue] -> a

runQuery :: IConnection c => c -> String -> IO [[SqlValue]]
runQuery con q = quickQuery con q []

populateTop :: IConnection c => c -> IO ()
populateTop con = do
    runQuery con "INSERT INTO top (name, msgs)\
                \ (SELECT name, COUNT(*) as count FROM messages\
                     \ GROUP BY name\
                     \ ORDER BY count DESC\
                     \ LIMIT 10);"
    return ()

getAndExtract :: IConnection c
              => c
              -> [String]
              -> Extract a
              -> String
              -> IO [a]
getAndExtract con qs f query = do
    sequence_ $ runQuery con <$> qs
    res <- runQuery con query
    return $ f <$> res

getRandMessages :: IConnection c => c -> IO [(String,String)]
getRandMessages con =
    let qs = ["SET @max = (SELECT MAX(id) FROM messages); "] in
    let q = "SELECT * FROM messages AS v\
           \ JOIN (SELECT ROUND(RAND() * @max) as v2\
                 \ FROM messages\
                 \ LIMIT 10) as dummy\
           \ ON v.id = v2;" in
    getAndExtract con qs extractMessage q

getKickers :: IConnection c => c -> IO [(String,Int)]
getKickers con =
    let q = "SELECT kicker, COUNT(*) as count FROM kicks\
           \ GROUP BY kicker\
           \ ORDER BY count DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractPair q

getKickees :: IConnection c => c -> IO [(String,Int)]
getKickees con =
    let q = "SELECT kickee, COUNT(*) as count FROM kicks\
           \ GROUP BY kickee\
           \ ORDER BY count DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractPair q

getUsers :: IConnection c => c -> IO [(String,Int)]
getUsers con =
    let q = "SELECT name, msgs FROM top;" in
    getAndExtract con [] extractPair q

getNicks :: IConnection c => c -> IO [(String,Int)]
getNicks con =
    let q = "SELECT oldname, COUNT(*) as count\
           \ FROM nickchanges\
           \ GROUP BY oldname\
           \ ORDER BY count DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractPair q

--SELECT * FROM messages AS v JOIN (SELECT ROUND(RAND() * @max) as v2 FROM messages LIMIT 10) as dummy ON v.id = v2
getMorning :: IConnection c
           => c
           -> IO ([(String,Int)],[(String,Int)],[(String,Int)],[(String,Int)])
getMorning con =
    let late = "SELECT n, COUNT(*) AS count, time\
              \ FROM messages JOIN (SELECT name AS n FROM top) AS dummy\
              \ WHERE HOUR(time) < 6 AND n = messages.name\
              \ GROUP BY messages.name\
              \ ORDER BY count DESC;" in
    let morn = "SELECT messages.name, COUNT(*) AS count, time\
              \ FROM messages JOIN (SELECT name AS n FROM top) AS dummy\
              \ WHERE HOUR(time) >= 6 AND HOUR(time) < 12\
                                    \ AND n = messages.name\
              \ GROUP BY messages.name\
              \ ORDER BY count DESC;" in
    let aftr = "SELECT messages.name, COUNT(*) AS count, time\
              \ FROM messages JOIN (SELECT name AS n FROM top) AS dummy\
              \ WHERE HOUR(time) >= 12 AND HOUR(time) < 18\
                                     \ AND n = messages.name\
              \ GROUP BY messages.name\
              \ ORDER BY count DESC;" in
    let eve = "SELECT messages.name, COUNT(*) AS count, time\
             \ FROM messages JOIN (SELECT name AS n FROM top) AS dummy\
             \ WHERE HOUR(time) >= 18 AND HOUR(time) < 24\
                                    \ AND n = messages.name\
             \ GROUP BY messages.name\
             \ ORDER BY count DESC;" in
    let get = getAndExtract con [] extractPair in
    (,,,) <$> get late
          <*> get morn
          <*> get aftr
          <*> get eve

getRandTopics :: IConnection c => c -> IO [(String,String)]
getRandTopics con =
    let qs = ["SET @max = (SELECT MAX(id) FROM topics);"] in
    let q = "SELECT * FROM topics AS v\
           \ JOIN (SELECT ROUND(RAND() * @max) as v2 FROM topics LIMIT 10) as dummy\
           \ ON v.id = v2;" in
    getAndExtract con qs extractTopic q


--getRandTopTen :: IConnection c => c -> IO [(String,String)]
--getRandTopTen con =

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
    sequence_ $ runQuery con <$> [ "DROP TABLE IF EXISTS messages;"
                                 , "DROP TABLE IF EXISTS statuses;"
                                 , "DROP TABLE IF EXISTS nickchanges;"
                                 , "DROP TABLE IF EXISTS topics;"
                                 , "DROP TABLE IF EXISTS kicks;"
                                 , "DROP TABLE IF EXISTS top;"
                                 ]
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
                                        \ PRIMARY KEY (id))\
                  \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    let nickchanges = "CREATE TABLE nickchanges(id BIGINT NOT NULL AUTO_INCREMENT,\
                                              \ oldname VARCHAR(36),\
                                              \ newname VARCHAR(36),\
                                              \ time DATETIME,\
                                              \ PRIMARY KEY (id))"
    let topics = "CREATE TABLE topics(id BIGINT NOT NULL AUTO_INCREMENT,\
                                    \ name VARCHAR(36),\
                                    \ topic VARCHAR(3000),\
                                    \ time DATETIME,\
                                    \ PRIMARY KEY (id))\
                \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    let kicks = "CREATE TABLE kicks(id BIGINT NOT NULL AUTO_INCREMENT,\
                                  \ kicker VARCHAR(36),\
                                  \ kickee VARCHAR(36),\
                                  \ reason VARCHAR(3000),\
                                  \ time DATETIME,\
                                  \ PRIMARY KEY (id))\
               \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    let top = "CREATE TABLE top(id INT NOT NULL AUTO_INCREMENT,\
                              \ name VARCHAR(36),\
                              \ msgs INT,\
                              \ PRIMARY KEY (id))\
             \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    sequence_ $ runQuery con <$> [ messages
                                 , statuses
                                 , nickchanges
                                 , topics
                                 , kicks
                                 , top
                                 ]
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



zipAssoc :: Ord a => [(a,b)] -> [(a, [b])]
zipAssoc xs = MMap.assocs $ MMap.fromList xs

combineUsage :: [(String,Int)]
             -> [(String,Int)]
             -> [(String,Int)]
             -> [(String,Int)]
             -> [(String,Int)]
             -> [(String,Int,Int,Int,Int,Int)]
combineUsage late morn aftr evening users =
    combine <$> users
    where combine (user,msgs) =
           let look s = case lookup user s of
                        Just x -> x
                        Nothing -> 0 in
           let w = look late
               x = look morn
               y = look aftr
               z = look evening in
           let ddiv :: Int -> Int -> Float
               ddiv = (/) `on` fromIntegral in
           let total = (w + x + y + z) in
           let percent t = truncate $ ddiv (t * 100) total in
           (user, percent w, percent x, percent y, percent z, msgs)



generate :: IConnection c => c -> IO ()
generate con = do
    populateTop con
    let headerList s xs = withHeading s $ makeList xs
    users <- getUsers con
    (late, morning, evening, night) <- getMorning con
    let times = formatTimes <$> combineUsage late morning evening night users
    rand <- formatList <$> getRandMessages con
    nicks <- formatList <$> getNicks con
    kickers <- formatList <$> getKickers con
    kickees <- formatList <$> getKickees con
    topics <- formatList <$> getRandTopics con
    let rendered = unlines $ (uncurry headerList) <$> [ ("Top Users", times)
                                                      , ("Random Messages", rand)
                                                      , ("Most Changed Nicks", nicks)
                                                      , ("Prolific Kickers", kickers)
                                                      , ("Trouble Makers", kickees)
                                                      , ("Topics", topics)
                                                      ]
    writeFile "generated.html" $ makeFile rendered

doAction :: Action -> IO ()
doAction action = do
    con <- connect
    case action of
        Repopulate -> repopulateDb con
        Generate -> generate con
    disconnect con
