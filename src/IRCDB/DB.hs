{-# LANGUAGE DoAndIfThenElse, NoMonomorphismRestriction, BangPatterns, TypeSynonymInstances #-}
module IRCDB.DB where

import Prelude hiding (foldl, concat, sequence_, sum)
import Control.Applicative
import Control.DeepSeq
import Control.Arrow
import Database.HDBC
import Database.HDBC.ODBC
import Data.Foldable
import Data.Function
import Data.Time.LocalTime
import Data.List (genericLength)
import System.Directory
import qualified Text.Regex.Posix as RE
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

getIndex :: IConnection c => c -> SqlValue -> IO SqlValue
getIndex con name = do
    m <- quickQuery con "SELECT count FROM counts WHERE name=?;" [name]
    case m of
        [(x:_)] -> return x
        _ -> return $ toSql (0 :: Int)

average :: (a -> Double) -> [a] -> Double
average f xs =
    let len = genericLength xs :: Double in
    if len == 0
    then 0
    else sum (f <$> xs) / len :: Double

insert :: IConnection c => LocalTime -> DataLine -> c -> IO LocalTime
insert t (Message time typ name msg) con = do
    let newT = setHoursMinutes t time
    let sqlName = toSql name
    let sqlType = toSql typ
    let sqlMsg = toSql msg
    let sqlTime = toSql newT

    let qq = "UPDATE counts\
            \ SET count=count+1, firstseen=(\
              \ CASE WHEN (DATEDIFF(?, lastseen) > 365)\
                   \ THEN ?\
                   \ ELSE firstseen\
              \ END), lastseen=?\
            \ WHERE name=?;"


    index <- getIndex con sqlName
    let words' = words msg
    let wordcount = toSql $ length words'
    let wordlength = toSql $ average genericLength words'

    prepared <- prepare con "INSERT INTO messages (name, type, userindex, wordcount, wordlength, text, time)\
                           \ VALUES (?,?,?,?,?,?,?);"

    execute prepared [sqlName, sqlType, index, wordcount, wordlength, sqlMsg, sqlTime]
    case fromSql index == (0 :: Int) of
        True -> do quickQuery con "INSERT INTO counts (name, count, lastseen, firstseen)\
                                 \ VALUES (?,?,?,?)" [sqlName, toSql (1 :: Int), sqlTime, sqlTime]
                   return newT
        False -> do countQ <- prepare con qq
                    execute countQ [sqlTime, sqlTime, sqlTime, sqlName]
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
extractPair       _ = ("Error extracting pair", 0)

extractTopic :: [SqlValue] -> (String, String)
extractTopic (_:name:msg:_) = (fromSql name, fromSql msg)
extractTopic              _ = ("Error extracting topic", "")

extractMessage :: [SqlValue] -> (String, String)
extractMessage (_:msg:_:_:_:name:_) = (fromSql name, fromSql msg)
extractMessage                  _ = ("Error extracting message", "")


type Extract a = [SqlValue] -> a

runQuery :: IConnection c => c -> String -> IO [[SqlValue]]
runQuery con q = quickQuery con q []

populateTop :: IConnection c => c -> IO ()
populateTop con = do
    runQuery con "TRUNCATE top;"
    runQuery con "INSERT INTO top (name, msgs)\
                \ (SELECT name, COUNT(*) AS count\
                 \ FROM messages\
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

-- a nick is "unique" if it has over N messages and doesnt have an oldnick such that
-- numMessages(oldNick) => numMessages(nick)
populateUnique :: IConnection c => c -> IO ()
populateUnique con = do
    runQuery con "TRUNCATE uniquenicks;"
    let q = "INSERT INTO uniquenicks (name, count)\
           \ (SELECT DISTINCT newname, c.count\
           \ FROM nickchanges AS v\
           \ INNER JOIN counts as c\
           \ ON c.count > 100 AND c.name = v.newname\
           \ WHERE (ISNULL((SELECT newname\
                          \ FROM nickchanges\
                          \ WHERE newname = v.oldname\
                          \ LIMIT 1))\
               \ OR ISNULL((SELECT count AS cc\
                          \ FROM counts\
                          \ WHERE name = v.oldname\
                          \ HAVING cc / 10 < c.count))))"
    runQuery con q
    return ()

getUniqueNicks :: IConnection c => c -> IO [(String,Int)]
getUniqueNicks con =
    let q = "SELECT name, count\
           \ FROM uniquenicks\
           \ ORDER BY count DESC" in
    let extract (s:c:_) = (fromSql s, fromSql c) in
    getAndExtract con [] extract q


getOverallActivity :: IConnection c => c -> IO [(Int,Int)]
getOverallActivity con = do
    let q = "SELECT HOUR(messages.time) AS h, COUNT(*)\
           \ FROM messages\
           \ GROUP BY h\
           \ ORDER BY h;"
    let extract (_:y:_) = fromSql y
    times <- runQuery con q
    return $ zip [0..] (extract <$> times)

getRandMessages :: IConnection c => c -> IO [(String, String)]
getRandMessages con =
    let qs = ["SET @max = (SELECT MAX(id) FROM messages); "] in
    let q = "SELECT *\
           \ FROM messages AS v\
           \ JOIN (SELECT ROUND(RAND() * @max) AS v2\
                 \ FROM messages\
                 \ LIMIT 10) AS dummy\
           \ ON v.id = v2;" in
    getAndExtract con qs extractMessage q

getRandTopTen :: IConnection c => c -> IO [(String, String)]
getRandTopTen con = do
    let q = "SELECT * FROM messages AS v\
           \ INNER JOIN (SELECT ROUND(RAND() * msgs) as r, name, msgs\
                       \ FROM top) AS t\
           \ ON v.name = t.name AND v.userindex = r"

    getAndExtract con [] extractMessage q

getKickers :: IConnection c => c -> IO [(String, Int)]
getKickers con =
    let q = "SELECT kicker, COUNT(*) AS count\
           \ FROM kicks\
           \ GROUP BY kicker\
           \ ORDER BY count DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractPair q

getKickees :: IConnection c => c -> IO [(String, Int)]
getKickees con =
    let q = "SELECT kickee, COUNT(*) AS count\
           \ FROM kicks\
           \ GROUP BY kickee\
           \ ORDER BY count DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractPair q

getUsers :: IConnection c => c -> IO [(String, Int)]
getUsers con =
    let q = "SELECT name, msgs FROM top" in
    getAndExtract con [] extractPair q

getNicks :: IConnection c => c -> IO [(String, Int)]
getNicks con =
    let q = "SELECT oldname, COUNT(*) AS count\
           \ FROM nickchanges\
           \ GROUP BY oldname\
           \ ORDER BY count DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractPair q

urlRegexp :: String
urlRegexp = "http://[^ ]*"

extractSqlUrl :: [SqlValue] -> (String, String)
extractSqlUrl (x:y:_) = (fromSql x, fromSql y)

extractUrl :: String -> String
extractUrl s = case s RE.=~ urlRegexp :: [[String]] of
    ((x:_) : _) -> x
    _ -> "Error extracting url"

getUrls :: IConnection c => c -> IO [(String, String)]
getUrls con = do
    prepared <- prepare con "SELECT DISTINCT name, text\
                           \ FROM messages\
                           \ WHERE text REGEXP ?\
                           \ ORDER BY RAND()\
                           \ LIMIT 10"
    execute prepared [toSql urlRegexp]
    rows <- fetchAllRows' prepared
    let r = (second (linkify.extractUrl)) <$> extractSqlUrl <$> rows
    return r

getAverage :: IConnection c => c -> IO [(String,Int)]
getAverage con =
    let q = "SELECT messages.name, CAST(AVG(wordcount) AS UNSIGNED)\
           \ FROM messages\
           \ JOIN top as t\
           \ ON t.name = messages.name" in
    getAndExtract con [] extractPair q

getTimes :: IConnection c
         => c
         -> IO ([(String, Int)],[(String, Int)],[(String, Int)],[(String, Int)])
getTimes con = do
    let late = "SELECT messages.name, COUNT(*) AS count, time\
              \ FROM messages\
              \ JOIN top AS t\
              \ WHERE HOUR(time) < 6 AND t.name = messages.name\
              \ GROUP BY messages.name\
              \ ORDER BY count DESC;"
    let morn = "SELECT messages.name, COUNT(*) AS count, time\
              \ FROM messages\
              \ JOIN top AS t\
              \ WHERE HOUR(time) >= 6 AND HOUR(time) < 12\
                                    \ AND t.name = messages.name\
              \ GROUP BY messages.name\
              \ ORDER BY count DESC;"
    let aftr = "SELECT messages.name, COUNT(*) AS count, time\
              \ FROM messages\
              \ JOIN top AS t\
              \ WHERE HOUR(time) >= 12 AND HOUR(time) < 18\
                                     \ AND t.name = messages.name\
              \ GROUP BY messages.name\
              \ ORDER BY count DESC;"
    let eve = "SELECT messages.name, COUNT(*) AS count, time\
             \ FROM messages\
             \ JOIN top AS t\
             \ WHERE HOUR(time) >= 18 AND HOUR(time) < 24\
                                    \ AND t.name = messages.name\
             \ GROUP BY messages.name\
             \ ORDER BY count DESC;"
    let get' = getAndExtract con [] extractPair
    (,,,) <$> get' late
          <*> get' morn
          <*> get' aftr
          <*> get' eve

getRandTopics :: IConnection c => c -> IO [(String, String)]
getRandTopics con =
    let qs = ["SET @max = (SELECT MAX(id) FROM topics);"] in
    let q = "SELECT DISTINCT * FROM topics AS v\
           \ JOIN (SELECT ROUND(RAND() * @max) AS r\
                 \ FROM topics\
                 \ LIMIT 10) AS dummy\
           \ ON v.id = r;" in
    getAndExtract con qs extractTopic q

connect :: IO Connection
connect = do
    let connectionString = "DSN=name32;\
                          \ Driver={MySQL ODBC 5.3 ANSI Driver};\
                          \ Server=localhost;\
                          \ Port=3306;\
                          \ Database=testdb;\
                          \ User=root;\
                          \ Password=password;\
                          \ Option=3;"
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
                                 , "DROP TABLE IF EXISTS counts;"
                                 , "DROP TABLE IF EXISTS uniquenicks;"
                                 ]
    return ()

createDbs :: IConnection c => c -> IO ()
createDbs con = do
    let messages = "CREATE TABLE messages(id BIGINT NOT NULL AUTO_INCREMENT,\
                                        \ text VARCHAR(4000),\
                                        \ type INT,\
                                        \ userindex INT,\
                                        \ wordcount INT,\
                                        \ wordlength DOUBLE,\
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
    let count = "CREATE TABLE counts(id INT NOT NULL AUTO_INCREMENT,\
                                   \ name VARCHAR(36),\
                                   \ count INT,\
                                   \ lastseen TIME,\
                                   \ firstseen TIME,\
                                   \ PRIMARY KEY (id));"
    let unique = "CREATE TABLE uniquenicks(name VARCHAR(36),\
                                         \ count INT,\
                                         \ PRIMARY KEY (name));"


    sequence_ $ runQuery con <$> [ messages
                                 , statuses
                                 , nickchanges
                                 , topics
                                 , kicks
                                 , top
                                 , count
                                 , unique
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

combineUsage :: [(String, Int)]
             -> [(String, Int)]
             -> [(String, Int)]
             -> [(String, Int)]
             -> [(String, Int)]
             -> [(String, String)]
             -> [(String, Int, Int, Int, Int, Int, String)]
combineUsage late morn aftr evening users messages =
    combine <$> users
    where combine (user,ct) =
           let look s d = case lookup user s of
                              Just x -> x
                              Nothing -> d in
           let w = look late 0
               x = look morn 0
               y = look aftr 0
               z = look evening 0
               o = look messages "ERROR" in
           let ddiv :: Int -> Int -> Float
               ddiv = (/) `on` fromIntegral in
           let total = (w + x + y + z) in
           let percent t = truncate $ ddiv (t * 100) total in
           (user, percent w, percent x, percent y, percent z, ct, o)

generate :: IConnection c => c -> IO ()
generate con = do
    populateTop con
    populateUnique con
    users <- force <$> getUsers con

    (late, morning, evening, night) <- force <$> getTimes con
    randTop <- getRandTopTen con

    let userTimes = formatUserTimes $ combineUsage late morning evening night users randTop
    !rand <- getRandMessages con
    !nicks <- getNicks con
    !kickers <- getKickers con
    !kickees <- getKickees con
    !topics <- getRandTopics con
    !urls <- getUrls con
    !activity <- getOverallActivity con
    !unique <- getUniqueNicks con
    !avg <- getAverage con
    let rendered = unlines $ [ makeTimeScript "Activity" activity
                             , headerTable "Average Words" ("Name", "Average") avg
                             , headerTable "Unique Nicks" ("Name","Messages") unique
                             , headerTable "Some Random URLs" ("Name", "URL") urls
                             , withHeading "Top Users" $ userTimes
                             , headerTable "Random Messages" ("Name", "Message") rand
                             , headerTable "Most Changed Nicks" ("Name", "Times Changed") nicks
                             , headerTable "Prolific Kickers" ("Name", "Times kicking") kickers
                             , headerTable "Trouble Makers" ("Name", "Times Kicked") kickees
                             , headerTable "Topics" ("Name", "Topic") topics
                             ]
    writeFile "generated.html" $ makeFile rendered "css.css" ["util.js"]

doAction :: Action -> IO ()
doAction action = do
    con <- connect
    case action of
        Repopulate -> repopulateDb con
        Generate -> generate con
    disconnect con
