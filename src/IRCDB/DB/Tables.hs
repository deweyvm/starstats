module IRCDB.DB.Tables where

import Prelude hiding (readFile)
import Control.Applicative
import Control.Exception
import Data.ByteString(readFile)
import Data.Foldable(foldlM)
import Data.List(isInfixOf)
import Data.Time.LocalTime
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

getCount :: IConnection c => c -> IO Int
getCount con = do
    m <- quickQuery con "SELECT COUNT(*) FROM allusers;" []
    case m of
        [(x:_)] -> return $ fromSql x
        _ -> return 0

data DbInsert = DbInsert LocalTime Int (Maybe String) Int

processOne :: IConnection c
           => c
           -> DbInsert
           -> Either DbParseError DataLine
           -> IO DbInsert
processOne _ (DbInsert t ct prevName repCt) (Left (DbParseError ln s err)) = do
    putStrLn ("Line " ++ show ln)
    print s
    print err
    return (DbInsert t (ct+1) prevName repCt)
processOne con dbi@(DbInsert t ct _ _) (Right l) = do
    if ct `mod` 100 == 0
        then do count <- getCount con
                putStrLn (show ct ++ "  " ++ show count)
        else return ()
    hFlush stdout
    e <- try (insert dbi l con) :: IO (Either SqlError DbInsert)
    case e of
        Left l' -> do
            if (isInfixOf "Data too long" (show l'))
                then do print l'
                        return (DbInsert t (ct + 1) Nothing 1)
                else do print l'
                        error "error"
                        --return (t, ct + 1, Nothing, 1)
        Right r -> return r

insert :: IConnection c
       => DbInsert
       -> DataLine
       -> c
       -> IO DbInsert
insert (DbInsert t ct prevName repCt) (Message time typ name msg) con = do
    let newT = setHoursMinutes t time
    let newRep = if (prevName == Just name) then repCt + 1 else 1
    let sqlName = toSql name
    let sqlType = toSql typ
    let sqlPre = toSql (take 24 msg)
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


    let qa = "INSERT INTO allmsgs (hash, contents, count, length, hasURL, isComplex)\
            \ VALUES (CRC32(?), ?, 1, ?, ? LIKE '%http://%', ? NOT LIKE '%http://%' AND ? > 12)\
            \ ON DUPLICATE KEY UPDATE count=count+1;"

    let len = toSql $ length msg
    msgQ <- prepare con qa
    execute msgQ [sqlMsg, sqlMsg, len, sqlMsg, sqlMsg, sqlMsg, len]


    let qq = "INSERT INTO counts (name, msgcount, wordcount, charcount, lastseen, firstseen, isExclamation, isQuestion, isAmaze, isTxt, isNaysay, isApostrophe, isCaps, q1, q2, q3, q4, timesMentioned, timesMentioning) \
            \ VALUES (?, 0, 0, 0, ?, ?, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)\
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
            \     isExclamation=isExclamation+(IF(? LIKE '%!%', 1, 0)),\
            \     isQuestion=isQuestion+(IF(? LIKE '%?', 1, 0)),\
            \     isAmaze=isAmaze+(IF(? LIKE '%wow%' AND ? REGEXP '[[:<:]]wow[[:>:]]|really.?$', 1, 0)),\
            \     isTxt=isTxt+(IF(? REGEXP '[[:<:]](wat|wot|r|u|k|idk|ikr|v)[[:>:]]', 1, 0)),\
            \     isNaysay=isNaysay+(IF(? LIKE '%no%' AND ? REGEXP '[[:<:]]no[[:>:]]', 1, 0)),\
            \     isApostrophe=isApostrophe+(IF(? LIKE '%''%', 1, 0)),\
            \     isCaps=isCaps+(IF(? = BINARY UPPER(?), 1, 0)),\
            \     q1=q1+(IF(FLOOR(HOUR(?)/6) = 0, 1, 0)),\
            \     q2=q2+(IF(FLOOR(HOUR(?)/6) = 1, 1, 0)),\
            \     q3=q3+(IF(FLOOR(HOUR(?)/6) = 2, 1, 0)),\
            \     q4=q4+(IF(FLOOR(HOUR(?)/6) = 3, 1, 0))"

    countQ <- prepare con qq
    execute countQ [ sqlName, sqlTime, sqlTime
                   , sqlTime
                   , sqlTime
                   , sqlTime
                   , toSql wordcount
                   , toSql charcount
                   , sqlMsg
                   , sqlMsg
                   , sqlMsg, sqlMsg
                   , sqlMsg
                   , sqlMsg, sqlMsg
                   , sqlMsg
                   , sqlMsg, sqlMsg
                   , sqlTime
                   , sqlTime
                   , sqlTime
                   , sqlTime
                   ]

    let qt1 = "INSERT INTO totals (dummy, msgcount, wordcount, startDate, endDate)\
             \ VALUES (1,1, ?, ?,?)\
             \ ON DUPLICATE KEY UPDATE endDate=?"
    totalsQ1 <- prepare con qt1
    execute totalsQ1 [wordcount, sqlTime, sqlTime, sqlTime]

    let qt2 = "UPDATE totals\
             \ SET wordcount=wordcount+?,\
             \     msgcount=msgcount+1"
    totalsQ2 <- prepare con qt2
    execute totalsQ2 [wordcount]

    let qMentioned = "UPDATE counts \
                    \ JOIN allusers as u\
                    \ ON counts.name = u.name \
                    \ SET timesMentioned=timesMentioned+(IF(? REGEXP CONCAT('[[:<:]]', REPLACE(u.name, '|', '\\|'), '[[:>:]]'), 1, 0))"
    mentionedQ <- prepare con qMentioned
    execute mentionedQ [sqlMsg]

    -- fixme -- doesnt account for multiple mentions in one message
    let qMentioning = "UPDATE counts\
                     \ JOIN allusers as u\
                     \ ON ? REGEXP CONCAT('[[:<:]]', REPLACE(u.name, '|', '\\|'), '[[:>:]]')\
                     \ SET timesMentioning=timesMentioning+1\
                     \ WHERE counts.name = ?"
    mentioningQ <- prepare con qMentioning
    execute mentioningQ [sqlMsg, sqlName]

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
             \ SET count=count+IF(? REGEXP CONCAT('[[:<:]]', REPLACE(u.name, '|', '\\|'), '[[:>:]]'), 1, 0)\
             \ WHERE mentioner=? AND mentionee=u.name AND mentioner != mentionee"
    mention2 <- prepare con qqp
    execute mention2 [sqlMsg, sqlName]

    let qurl = "INSERT INTO urls (name, contents)\
              \ (SELECT ?, ?\
              \  FROM DUAL\
              \  WHERE ? LIKE '%http://%'\
              \ LIMIT 1);"
    urlQ <- prepare con qurl
    execute urlQ [sqlName, sqlMsg, sqlMsg]

    index <- getIndex con sqlName
    let qm = "INSERT INTO messages (name, type, userindex, wordcount, charcount, contents, contentspre, time, hour, quartile, hash)\
            \ VALUES (?,?,?,?,?,?,?,?,\
                    \ HOUR(?),\
                    \ HOUR(?)/6,\
                    \ CRC32(?));"
    message <- prepare con qm
    execute message [sqlName, sqlType, index, wordcount, charcount, sqlMsg, sqlPre, sqlTime
                    , sqlTime
                    , sqlTime
                    , sqlMsg
                    , sqlMsg, sqlMsg
                    , sqlMsg, sqlMsg
                    , sqlMsg]
    return (DbInsert newT (ct+1) (Just name) newRep)
insert (DbInsert t ct prevName repCt) (Nick time old new) con = do
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO nickchanges (oldname, newname, time)\
                           \ VALUES (?,?,?);"
    let sqlOld = toSql old
    let sqlMsg = toSql new
    let sqlTime = toSql newT
    execute prepared [sqlOld, sqlMsg, sqlTime]
    return (DbInsert newT (ct+1) prevName repCt)
insert (DbInsert t ct prevName repCt) (Kick time kickee kicker reason) con = do
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO kicks (kicker, kickee, reason, time)\
                           \ VALUES (?,?,?, ?);"
    let sqlKicker = toSql kicker
    let sqlKickee = toSql kickee
    let sqlReason = toSql reason
    let sqlTime = toSql newT
    execute prepared [sqlKicker, sqlKickee, sqlReason, sqlTime]
    return (DbInsert newT (ct+1) prevName repCt)
insert (DbInsert t ct prevName repCt) (Topic time setter topic) con = do
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO topics (name, topic, time)\
                           \ VALUES (?,?,?);"
    let sqlName = toSql setter
    let sqlTopic = toSql topic
    let sqlTime = toSql newT
    execute prepared [sqlName, sqlTopic, sqlTime]
    return (DbInsert newT (ct+1) prevName repCt)
insert (DbInsert _ ct prevName repCt) (Day date) _ = return (DbInsert date (ct+1) prevName repCt)
insert (DbInsert _ ct prevName repCt) (Open date) _ = return (DbInsert date (ct+1) prevName repCt)
insert (DbInsert t ct prevName repCt) _ _ = return (DbInsert t (ct+1) prevName repCt)

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
                                 , "DROP TABLE IF EXISTS urls;"
                                 , "DROP TABLE IF EXISTS totals;"
                                 ]
    return ()

createDbs :: IConnection c => c -> IO ()
createDbs con = do
    let messages = "CREATE TABLE messages(id INT NOT NULL AUTO_INCREMENT,\
                                        \ contents VARCHAR(500) NOT NULL,\
                                        \ contentspre VARCHAR(100) NOT NULL,\
                                        \ type INT NOT NULL,\
                                        \ userindex INT NOT NULL,\
                                        \ wordcount INT NOT NULL,\
                                        \ charcount INT NOT NULL,\
                                        \ name VARCHAR(36) NOT NULL,\
                                        \ time DATETIME NOT NULL,\
                                        \ hour TINYINT UNSIGNED NOT NULL,\
                                        \ quartile TINYINT UNSIGNED NOT NULL,\
                                        \ hash INT UNSIGNED NOT NULL,\
                                        \ PRIMARY KEY (id),\
                                        \ KEY (hash),\
                                        \ INDEX (contentspre),\
                                        \ INDEX (name, hour, quartile),\
                                        \ INDEX (name, userindex))\
                  \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    let seqcount = "CREATE TABLE seqcount(id INT NOT NULL AUTO_INCREMENT,\
                                        \ name VARCHAR(36) NOT NULL,\
                                        \ num INT NOT NULL,\
                                        \ PRIMARY KEY (id));"
    let statuses = "CREATE TABLE statuses(id INT NOT NULL AUTO_INCREMENT,\
                                        \ contents VARCHAR(500) NOT NULL,\
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
                                   \ timesMentioned INT NOT NULL,\
                                   \ timesMentioning INT NOT NULL,\
                                   \ isExclamation INT NOT NULL,\
                                   \ isQuestion INT NOT NULL,\
                                   \ isAmaze INT NOT NULL,\
                                   \ isTxt INT NOT NULL,\
                                   \ isNaysay INT NOT NULL,\
                                   \ isApostrophe INT NOT NULL,\
                                   \ isCaps Int NOT NULL,\
                                   \ q1 INT NOT NULL,\
                                   \ q2 INT NOT NULL,\
                                   \ q3 INT NOT NULL,\
                                   \ q4 INT NOT NULL,\
                                   \ PRIMARY KEY (name));"
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
                                      \ isComplex BOOL NOT NULL,\
                                      \ hash CHAR(50) NOT NULL,\
                                      \ PRIMARY KEY (hash))\
                 \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"

    let mentions = "CREATE TABLE mentions(id INT NOT NULL AUTO_INCREMENT,\
                                        \ mentioner VARCHAR(36) NOT NULL,\
                                        \ mentionee VARCHAR(36) NOT NULL,\
                                        \ count INT NOT NULL,\
                                        \ PRIMARY KEY (mentioner, mentionee),\
                                        \ KEY (id));"
    let urls = "CREATE TABLE urls(id INT NOT NULL AUTO_INCREMENT,\
                                \ name VARCHAR(36) NOT NULL,\
                                \ contents VARCHAR(500) NOT NULL,\
                                \ PRIMARY KEY (id));"
    let totals = "CREATE TABLE totals(dummy INT NOT NULL,\
                                    \ wordcount INT NOT NULL,\
                                    \ msgcount INT NOT NULL,\
                                    \ startDate DATETIME NOT NULL,\
                                    \ endDate DATETIME NOT NULL,\
                                    \ PRIMARY KEY (dummy));"
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
                                 , urls
                                 , totals
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
    let seed = DbInsert time 0 Nothing 1
    foldlM (processOne con) seed parsed
    commit con

repopulateDb :: IConnection c => c -> IO ()
repopulateDb con = do
    deleteDbs con
    createDbs con
    populateDbs con
