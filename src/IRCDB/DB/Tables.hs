module IRCDB.DB.Tables where

import Control.Applicative
import Data.Foldable(foldlM)
import Data.Time.LocalTime
import Database.HDBC

import IRCDB.DB.Utils
import IRCDB.Parser
import IRCDB.Time

getIndex :: IConnection c => c -> SqlValue -> IO SqlValue
getIndex con name = do
    m <- quickQuery con "SELECT count FROM counts WHERE name=?;" [name]
    case m of
        [(x:_)] -> return x
        _ -> return $ toSql (0 :: Int)

getCount :: IConnection c => c -> SqlValue -> IO SqlValue
getCount con name = do
    m <- quickQuery con "SELECT count FROM mentions WHERE name=?;" [name]
    case m of
        [(x:_)] -> return x
        _ -> return $ toSql (0 :: Int)

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
    let sqlName = toSql name
    let sqlType = toSql typ
    let sqlMsg = toSql msg
    let sqlTime = toSql (subHours newT (subtract 3))

    let qq = "UPDATE counts\
            \ SET count=count+1, firstseen=(\
              \ CASE WHEN (DATEDIFF(?, lastseen) > 365)\
                   \ THEN ?\
                   \ ELSE firstseen\
              \ END), lastseen=?\
            \ WHERE name=?;"

    let qu = "INSERT INTO allusers (name)\
            \ VALUES (?)\
            \ ON DUPLICATE KEY UPDATE name=name;"

    let qp = "INSERT INTO mentions (mentioner, mentionee, count)\
            \ (SELECT ?, name, 0 FROM allusers)\
            \ ON DUPLICATE KEY UPDATE count=count"
    let qqp = "UPDATE mentions\
             \ JOIN allusers AS u\
             \ SET count=IF(? REGEXP CONCAT('[[:<:]]', REPLACE(u.name, '|', '\\''), '[[:>:]]'), count + 1, count)\
             \ WHERE mentioner=? AND mentionee=u.name AND mentioner != mentionee"



    index <- getIndex con sqlName
    let words' = words msg
    let wordcount = toSql $ length words'
    let stripped = words $ replace urlRegexp "" msg
    let charcount = toSql $ sum $ length <$> stripped -- fixme : this could be more precise

    prepared <- prepare con "INSERT INTO messages (name, type, userindex, wordcount, charcount, text, time)\
                           \ VALUES (?,?,?,?,?,?,?);"

    mention <- prepare con qp
    mention2 <- prepare con qqp
    users <- prepare con qu
    execute prepared [sqlName, sqlType, index, wordcount, charcount, sqlMsg, sqlTime]
    execute users [sqlName]
    execute mention [sqlName]
    execute mention2 [sqlMsg, sqlName]
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

-- a nick is "unique" if it has over N messages and doesnt have an oldnick such that
-- numMessages(oldNick) => numMessages(nick)
populateUnique :: IConnection c => c -> IO ()
populateUnique con = do
    runQuery con "TRUNCATE uniquenicks;"
    let q = "INSERT INTO uniquenicks (name, count)\
           \ (SELECT DISTINCT newname, c.count\
           \ FROM nickchanges AS v\
           \ INNER JOIN counts AS c\
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
                                 , "DROP TABLE IF EXISTS mentions;"
                                 , "DROP TABLE IF EXISTS allusers;"
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
    let unique = "CREATE TABLE uniquenicks(id INT NOT NULL AUTO_INCREMENT,\
                                         \ name VARCHAR(36),\
                                         \ count INT,\
                                         \ PRIMARY KEY (id));"
    let allusers = "CREATE TABLE allusers(name VARCHAR(36),\
                                        \ PRIMARY KEY (name));"
    let mentions = "CREATE TABLE mentions(id INT NOT NULL AUTO_INCREMENT,\
                                        \ mentioner VARCHAR(36) DEFAULT \"\",\
                                        \ mentionee VARCHAR(36) DEFAULT \"\",\
                                        \ count INT DEFAULT 0,\
                                        \ PRIMARY KEY (mentioner, mentionee),\
                                        \ KEY (id));"

    sequence_ $ runQuery con <$> [ messages
                                 , statuses
                                 , nickchanges
                                 , topics
                                 , kicks
                                 , top
                                 , count
                                 , unique
                                 , mentions
                                 , allusers
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
