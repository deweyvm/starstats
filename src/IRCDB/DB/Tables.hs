module IRCDB.DB.Tables where

import Prelude hiding (readFile)
import Control.Applicative
import Data.Foldable(foldlM)
import Data.Time.LocalTime
import Data.ByteString(readFile)
import Data.Text(unpack)
import Data.Text.Encoding(decodeUtf8)
import Database.HDBC
import System.IO hiding (readFile)

import IRCDB.DB.Utils
import IRCDB.Parser
import IRCDB.Time

getIndex :: IConnection c => c -> SqlValue -> IO SqlValue
getIndex con name = do
    m <- quickQuery con "SELECT msgcount FROM counts WHERE name=?;" [name]
    case m of
        [(x:_)] -> return x
        _ -> return $ toSql (0 :: Int)

processOne :: IConnection c
           => c
           -> (LocalTime, Int, Maybe String, Int)
           -> Either (Int, String, String) DataLine
           -> IO (LocalTime, Int, Maybe String, Int)
processOne _ (t, ct, prevName, repCt) (Left (ln, s, err)) = do
    putStrLn ("Line " ++ show ln)
    print s
    print err
    return (t, ct+1, prevName, repCt)
processOne con (t, ct, prevName, repCt) (Right l) = do
    if ct `mod` 100 == 0 then print ct else return ()
    hFlush stdout
    insert t ct prevName repCt l con

insert :: IConnection c
       => LocalTime
       -> Int
       -> Maybe String
       -> Int
       -> DataLine
       -> c
       -> IO (LocalTime, Int, Maybe String, Int)
insert t ct prevName repCt (Message time typ name msg) con = do
    let newT = setHoursMinutes t time
    let newRep = if (prevName == Just name) then repCt + 1 else 1
    let sqlName = toSql name
    let sqlType = toSql typ
    let sqlPre = toSql (take (24) msg)
    let sqlMsg = toSql (take 500 msg)
    let sqlTime = toSql (subHours newT (subtract 3))

    let words' = words msg
    let wordcount = toSql $ length words'
    let stripped = words $ replace urlRegexp "" msg
    let charcount = toSql $ sum $ length <$> stripped -- fixme : this could be more precise
    let qs = "INSERT INTO seqcount (name, num)\
            \ VALUES (?, ?);"

    case (prevName, newRep) of
        (Just n, 1) | repCt > 5 -> do seqQ <- prepare con qs
                                      execute seqQ [toSql n, toSql repCt]
        _ -> return 1


    let qa = "INSERT INTO allmsgs (hash, contents, count, length, hasURL)\
            \ VALUES (CRC32(?), ?, 1, ?, ? LIKE '%http%')\
            \ ON DUPLICATE KEY UPDATE count=count+1;"

    let len = toSql $ length msg
    msgQ <- prepare con qa
    execute msgQ [sqlMsg, sqlMsg, len, sqlMsg, sqlMsg]

    let qq = "INSERT INTO counts (name, msgcount, wordcount, charcount, lastseen, firstseen, q1, q2, q3, q4) \
            \ VALUES (?, 0, 0, 0, ?, ?, 0, 0, 0, 0)\
            \ ON DUPLICATE KEY UPDATE\
            \     msgcount=msgcount+1, \
            \     firstseen=(\
            \         CASE WHEN (DATEDIFF(?, lastseen) > 365)\
            \              THEN ?\
            \              ELSE firstseen\
            \         END), \
            \     lastseen=?,\
            \     wordcount=wordcount+?,\
            \     charcount=charcount+?,\
            \     q1=q1+(IF(FLOOR(HOUR(?)/6) = 0, 1, 0)),\
            \     q2=q2+(IF(FLOOR(HOUR(?)/6) = 1, 1, 0)),\
            \     q3=q3+(IF(FLOOR(HOUR(?)/6) = 2, 1, 0)),\
            \     q4=q4+(IF(FLOOR(HOUR(?)/6) = 3, 1, 0))"
    countQ <- prepare con qq
    execute countQ [sqlName, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, toSql wordcount, toSql charcount, sqlTime, sqlTime, sqlTime, sqlTime]
    let qu = "INSERT INTO allusers (name)\
            \ VALUES (?)\
            \ ON DUPLICATE KEY UPDATE name=name;"
    users <- prepare con qu
    execute users [sqlName]

    let qp = "INSERT INTO mentions (mentioner, mentionee, count)\
            \ (SELECT ?, name, 0 FROM allusers)\
            \ ON DUPLICATE KEY UPDATE count=count"
    mention <- prepare con qp
    execute mention [sqlName]

    let qqp = "UPDATE mentions\
             \ JOIN allusers AS u\
             \ SET count=IF(? REGEXP CONCAT('[[:<:]]', REPLACE(u.name, '|', '\\''), '[[:>:]]'), count + 1, count)\
             \ WHERE mentioner=? AND mentionee=u.name AND mentioner != mentionee"
    mention2 <- prepare con qqp
    execute mention2 [sqlMsg, sqlName]


    index <- getIndex con sqlName
    let qm = "INSERT INTO messages (name, type, userindex, wordcount, charcount, text, textpre, time, hour, quartile, isTxt, hash, isCaps, isAmaze, isQuestion, isExclamation, hasNo, hasApostrophe)\
            \ VALUES (?,?,?,?,?,?,?,?,\
                    \ HOUR(?),\
                    \ HOUR(?)/6,\
                    \ ? REGEXP '[[:<:]](wat|wot|r|u|k|idk|ikr|v)[[:>:]]',\
                    \ CRC32(?),\
                    \ ? = BINARY UPPER(?),\
                    \ ? LIKE '%wow%' AND ? REGEXP '[[:<:]]wow[[:>:]]|really.?$',\
                    \ ? LIKE '%!%',\
                    \ ? LIKE '%?',\
                    \ ? LIKE '%no%' AND ? REGEXP '[[:<:]]no[[:>:]]',\
                    \ ? LIKE '%''%');"
    message <- prepare con qm
    execute message [sqlName, sqlType, index, wordcount, charcount, sqlMsg, sqlPre, sqlTime
                    , sqlTime
                    , sqlTime
                    , sqlMsg
                    , sqlMsg
                    , sqlMsg, sqlMsg
                    , sqlMsg, sqlMsg
                    , sqlMsg
                    , sqlMsg
                    , sqlMsg, sqlMsg
                    , sqlMsg]


    return (newT, ct+1, Just name, newRep)
insert t ct prevName newRep (Nick time old new) con = do
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO nickchanges (oldname, newname, time)\
                           \ VALUES (?,?,?);"
    let sqlOld = toSql old
    let sqlMsg = toSql new
    let sqlTime = toSql newT
    execute prepared [sqlOld, sqlMsg, sqlTime]
    return (newT, ct+1, prevName, newRep)
insert t ct prevName repCt (Kick time kickee kicker reason) con = do
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO kicks (kicker, kickee, reason, time)\
                           \ VALUES (?,?,?, ?);"
    let sqlKicker = toSql kicker
    let sqlKickee = toSql kickee
    let sqlReason = toSql reason
    let sqlTime = toSql newT
    execute prepared [sqlKicker, sqlKickee, sqlReason, sqlTime]
    return (newT, ct+1, prevName, repCt)
insert t ct prevName repCt (Topic time setter topic) con = do
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO topics (name, topic, time)\
                           \ VALUES (?,?,?);"
    let sqlName = toSql setter
    let sqlTopic = toSql topic
    let sqlTime = toSql newT
    execute prepared [sqlName, sqlTopic, sqlTime]
    return (newT, ct+1, prevName, repCt)
insert _ ct prevName repCt (Day date) _ = return (date, ct+1, prevName, repCt)
insert _ ct prevName repCt (Open date) _ = return (date, ct+1, prevName, repCt)
insert t ct prevName repCt _ _ = return (t, ct+1, prevName, repCt)

populateTop :: IConnection c => c -> IO ()
populateTop con = do
    runQuery con "TRUNCATE top;"
    runQuery con "INSERT INTO top (name, msgs)\
                \ (SELECT name, msgcount\
                 \ FROM counts\
                 \ ORDER BY msgcount DESC\
                 \ LIMIT 10);"
    return ()

-- a nick is "unique" if it has over N messages and doesnt have an oldnick such that
-- numMessages(oldNick) => numMessages(nick)
populateUnique :: IConnection c => c -> IO ()
populateUnique con = do
    runQuery con "TRUNCATE uniquenicks;"
    let q = "INSERT INTO uniquenicks (name, count)\
           \ (SELECT DISTINCT newname, c.msgcount\
           \ FROM nickchanges AS v\
           \ INNER JOIN counts AS c\
           \ ON c.msgcount > 100 AND c.name = v.newname\
           \ WHERE (ISNULL((SELECT newname\
                          \ FROM nickchanges\
                          \ WHERE newname = v.oldname\
                          \ LIMIT 1))\
               \ OR ISNULL((SELECT msgcount AS cc\
                          \ FROM counts\
                          \ WHERE name = v.oldname\
                          \ HAVING cc / 10 < c.msgcount))))"
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
                                 , "DROP TABLE IF EXISTS allmsgs;"
                                 , "DROP TABLE IF EXISTS seqcount;"
                                 ]
    return ()

createDbs :: IConnection c => c -> IO ()
createDbs con = do
    let messages = "CREATE TABLE messages(id INT NOT NULL AUTO_INCREMENT,\
                                        \ text VARCHAR(500) NOT NULL,\
                                        \ textpre VARCHAR(100) NOT NULL,\
                                        \ type INT NOT NULL,\
                                        \ userindex INT NOT NULL,\
                                        \ wordcount INT NOT NULL,\
                                        \ charcount INT NOT NULL,\
                                        \ name VARCHAR(36) NOT NULL,\
                                        \ time DATETIME NOT NULL,\
                                        \ hour TINYINT UNSIGNED NOT NULL,\
                                        \ quartile TINYINT UNSIGNED NOT NULL,\
                                        \ isTxt BOOL NOT NULL,\
                                        \ isCaps BOOL NOT NULL,\
                                        \ isAmaze BOOL NOT NULL,\
                                        \ isQuestion BOOL NOT NULL,\
                                        \ isExclamation BOOL NOT NULL,\
                                        \ hasApostrophe BOOL NOT NULL,\
                                        \ hasNo BOOL NOT NULL,\
                                        \ hash INT UNSIGNED NOT NULL,\
                                        \ PRIMARY KEY (id),\
                                        \ KEY (hash),\
                                        \ INDEX (textpre),\
                                        \ INDEX (name, isCaps),\
                                        \ INDEX (name, isTxt),\
                                        \ INDEX (name, isAmaze),\
                                        \ INDEX (name, isQuestion),\
                                        \ INDEX (name, isExclamation),\
                                        \ INDEX (name, hasNo),\
                                        \ INDEX (name, hasApostrophe),\
                                        \ INDEX (name, hour, quartile),\
                                        \ INDEX (name, userindex))\
                  \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    let seqcount = "CREATE TABLE seqcount(id INT NOT NULL AUTO_INCREMENT,\
                                        \ name VARCHAR(36) NOT NULL,\
                                        \ num INT NOT NULL,\
                                        \ PRIMARY KEY (id));"
    let statuses = "CREATE TABLE statuses(id INT NOT NULL AUTO_INCREMENT,\
                                        \ text VARCHAR(500) NOT NULL,\
                                        \ name VARCHAR(36) NOT NULL,\
                                        \ time DATETIME NOT NULL,\
                                        \ PRIMARY KEY (id))\
                  \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    let nickchanges = "CREATE TABLE nickchanges(id INT NOT NULL AUTO_INCREMENT,\
                                              \ oldname VARCHAR(36) NOT NULL,\
                                              \ newname VARCHAR(36) NOT NULL,\
                                              \ time DATETIME NOT NULL,\
                                              \ PRIMARY KEY (id))"
    let topics = "CREATE TABLE topics(id INT NOT NULL AUTO_INCREMENT,\
                                    \ name VARCHAR(36) NOT NULL,\
                                    \ topic VARCHAR(500) NOT NULL,\
                                    \ time DATETIME NOT NULL,\
                                    \ PRIMARY KEY (id))\
                \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    let kicks = "CREATE TABLE kicks(id INT NOT NULL AUTO_INCREMENT,\
                                  \ kicker VARCHAR(36) NOT NULL,\
                                  \ kickee VARCHAR(36) NOT NULL,\
                                  \ reason VARCHAR(500) NOT NULL,\
                                  \ time DATETIME NOT NULL,\
                                  \ PRIMARY KEY (id))\
               \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    let top = "CREATE TABLE top(id INT NOT NULL AUTO_INCREMENT,\
                              \ name VARCHAR(36) NOT NULL,\
                              \ msgs INT NOT NULL,\
                              \ PRIMARY KEY (id))\
             \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    let count = "CREATE TABLE counts(name VARCHAR(36) NOT NULL,\
                                   \ msgcount INT NOT NULL,\
                                   \ wordcount INT NOT NULL,\
                                   \ charcount INT NOT NULL,\
                                   \ lastseen TIME NOT NULL,\
                                   \ firstseen TIME NOT NULL,\
                                   \ q1 INT NOT NULL,\
                                   \ q2 INT NOT NULL,\
                                   \ q3 INT NOT NULL,\
                                   \ q4 INT NOT NULL,\
                                   \ PRIMARY KEY (name),\
                                   \ INDEX (name, msgcount));"
    let unique = "CREATE TABLE uniquenicks(id INT NOT NULL AUTO_INCREMENT,\
                                         \ name VARCHAR(36) NOT NULL,\
                                         \ count INT NOT NULL,\
                                         \ PRIMARY KEY (id));"
    let allusers = "CREATE TABLE allusers(name VARCHAR(36) NOT NULL,\
                                        \ PRIMARY KEY (name));"
    let allmsgs = "CREATE TABLE allmsgs(contents VARCHAR(500) NOT NULL,\
                                      \ count INT NOT NULL,\
                                      \ length INT NOT NULL,\
                                      \ hasURL BOOL NOT NULL,\
                                      \ hash CHAR(50) NOT NULL,\
                                      \ PRIMARY KEY (hash))\
                 \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"

    let mentions = "CREATE TABLE mentions(id INT NOT NULL AUTO_INCREMENT,\
                                        \ mentioner VARCHAR(36) NOT NULL,\
                                        \ mentionee VARCHAR(36) NOT NULL,\
                                        \ count INT NOT NULL,\
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
                                 , allmsgs
                                 , seqcount
                                 ]
    return ()

populateDbs :: IConnection c => c -> IO ()
populateDbs con = do
    logfile <- readConfig
    bytestring <- readFile logfile
    let utf8' = unpack $ decodeUtf8 bytestring
    let contents = lines utf8'
    let parsed = parseLine <$> zip [1..] contents
    let time = undefined
    foldlM (processOne con) (time, 0, Nothing, 1) parsed
    commit con

repopulateDb :: IConnection c => c -> IO ()
repopulateDb con = do
    deleteDbs con
    createDbs con
    populateDbs con
