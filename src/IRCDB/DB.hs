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
import Data.Maybe
import Data.Time.LocalTime
import Data.List (groupBy)
import System.Directory
import qualified Text.Regex.Posix as REP
import qualified Text.Regex as RE
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
    let stripped = words $ RE.subRegex (RE.mkRegex urlRegexp) msg ""
    let charcount = toSql $ sum $ length <$> stripped -- fixme : this could be more precise

    prepared <- prepare con "INSERT INTO messages (name, type, userindex, wordcount, charcount, text, time)\
                           \ VALUES (?,?,?,?,?,?,?);"

    execute prepared [sqlName, sqlType, index, wordcount, charcount, sqlMsg, sqlTime]
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
extractMessage (name:msg:_) = (fromSql name, fromSql msg)
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
    let q = "SELECT name, text\
           \ FROM messages AS m\
           \ JOIN (SELECT ROUND(RAND() * @max) AS m2\
                 \ FROM messages\
                 \ LIMIT 10) AS dummy\
           \ ON m.id = m2;" in
    getAndExtract con qs extractMessage q

getRandTopTen :: IConnection c => c -> IO [(String, String)]
getRandTopTen con = do
    let q = "SELECT m.name, text\
           \ FROM messages AS m\
           \ INNER JOIN (SELECT ROUND(RAND() * msgs) as r, name, msgs\
                       \ FROM top) AS t\
           \ ON m.name = t.name AND m.userindex = r"

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
extractUrl s = case s REP.=~ urlRegexp :: [[String]] of
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

getAverageWordCount :: IConnection c => c -> IO [(String, Double)]
getAverageWordCount con =
    let q = "SELECT m.name, AVG(m.wordcount) as avg\
           \ FROM top as t\
           \ INNER JOIN messages as m\
           \ ON t.name = m.name\
           \ GROUP BY m.name\
           \ ORDER BY avg DESC" in
    let extract (x:y:_) = (fromSql x, fromSql y) in
    getAndExtract con [] extract q

getAverageWordLength :: IConnection c => c -> IO [(String, Double)]
getAverageWordLength con =
    let q = "SELECT m.name, IFNULL(SUM(m.charcount)/SUM(m.wordcount), 0) as avg\
           \ FROM top as t\
           \ INNER JOIN messages as m\
           \ ON t.name = m.name\
           \ GROUP BY m.name\
           \ ORDER BY avg DESC" in
    let extract (x:y:_) = (fromSql x, fromSql y) in
    getAndExtract con [] extract q

--[ ("anomaly",0,4199)
--, ("anomaly",1,1640)
--, ("anomaly",3,2852)
--, ("Bartle",0,9534)
--, ("Bartle",1,2306)
--, ("Bartle",2,4383)
--, ("Bartle",3,11671)
--, ("Bwarch",0,1143)
--, ("Bwarch",1,247)
--, ("Bwarch",2,236)
--, ("Bwarch",3,1240)]
-- ->
-- [ ("anomaly", 4199, 1640,    0,  2852)
-- , ( "Bartle", 9534, 2306, 4383, 11671)
-- , ( "Bwarch", 1143,  247,  236,  1240)]
assemble :: [(String, Int, Int)] -> [[(String, Int, Int)]]
assemble xs = groupBy (\(n, _, _) (m, _, _) -> n == m) xs

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

assemble2 :: [[(String, Int, Int)]] -> [(String, Int, Int, Int, Int)]
assemble2 xs =
    let grabAll :: [(String, Int, Int)] -> (String, Int, Int, Int, Int)
        grabAll vs@((name,_,_):_) =
            let getWhere :: Int -> Int
                getWhere i = fromMaybe 0 (thd3 <$> (find (\(nn, ii, _) -> i == ii && name == nn) vs)) in
            let w = getWhere 0
                x = getWhere 1
                y = getWhere 2
                z = getWhere 3 in
            (name, w, x, y, z) in
    grabAll <$> xs


getTimes :: IConnection c
         => c
         -> IO [(String, Int, Int, Int, Int)]
getTimes con = do
    let all' = "SELECT messages.name, FLOOR(HOUR(time)/6) as h, COUNT(*) AS count\
              \ FROM messages\
              \ JOIN top AS t\
              \ ON t.name = messages.name\
              \ GROUP BY h, messages.name\
              \ ORDER BY messages.name, h, count DESC;"
    let extract (x:y:z:_) = (fromSql x, fromSql y, fromSql z) :: (String, Int, Int)
    xs <- getAndExtract con [] extract all'
    return $ (assemble2 . assemble) xs


toTimeBars :: [(String, Int, Int, Int, Int)] -> [(String, TimeBar)]
toTimeBars = ((\(user, w, x, y, z) -> (user, TimeBar user w x y z)) <$>)

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
                                        \ charcount INT,\
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
             -> [(String, TimeBar)]
             -> [(String, String)]
             -> [(String, TimeBar, Int, String)]
combineUsage users bars messages =
    combine <$> users
    where combine (user,ct) =
           let look s d = case lookup user s of
                              Just x -> x
                              Nothing -> d in
           let (TimeBar _ w x y z) = look bars (TimeBar "ERROR" (-1) (-1) (-1) (-1))
               o = look messages "ERROR" in
           let ddiv :: Int -> Int -> Float
               ddiv = (/) `on` fromIntegral in
           let total = (w + x + y + z) in
           let percent t = truncate $ ddiv (t * 100) total in
           (user, TimeBar user (percent w) (percent x) (percent y) (percent z), ct, o)

generate :: IConnection c => c -> IO ()
generate con = do
    populateTop con
    populateUnique con
    users <- force <$> getUsers con

    tups <- force <$> getTimes con
    randTop <- force <$> getRandTopTen con
    let bars = (toTimeBars tups)
    let userTimes = formatUserTimes $ combineUsage users bars randTop
    !rand <- getRandMessages con
    !nicks <- getNicks con
    !kickers <- getKickers con
    !kickees <- getKickees con
    !topics <- getRandTopics con
    !urls <- getUrls con
    !activity <- getOverallActivity con
    !unique <- getUniqueNicks con
    !avgwc <- getAverageWordCount con
    !avgwl <- getAverageWordLength con
    let rendered = unlines $ [ makeTimeScript "Activity" activity
                             , headerTable "Word Count" ("Name", "Average Word Count") avgwc
                             , headerTable "Word Length" ("Name", "Average Word Length") avgwl
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
