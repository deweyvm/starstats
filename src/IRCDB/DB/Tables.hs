module IRCDB.DB.Tables where

import Prelude hiding (readFile)
import Control.Applicative
import Control.Exception
import Control.DeepSeq
import qualified Criterion.Measurement as M
import Data.ByteString(readFile)
import Data.Foldable(foldlM)
import Data.List(isInfixOf)
import Data.Time.LocalTime
import Data.Text(unpack)
import Data.Text.Encoding(decodeUtf8)
import Data.Maybe(fromMaybe)
import Database.HDBC
import Text.Printf
import System.IO hiding (readFile)

import IRCDB.DB.Utils
import IRCDB.Parser
import IRCDB.Time

getCount :: IConnection c => c -> IO Int
getCount con = do
    m <- quickQuery con "SELECT COUNT(*) FROM activeusers;" []
    case m of
        [(x:_)] -> return $ fromSql x
        _ -> return 0

data DbInsert = DbInsert Int

processOne :: IConnection c
           => c
           -> (Double, DbInsert)
           -> Either DbParseError DataLine
           -> IO (Double, DbInsert)
processOne _ (d, (DbInsert ct)) (Left (DbParseError ln s err)) = do
    putStrLn ("Line " ++ show ln)
    print s
    print err
    return (d, (DbInsert (ct+1)))
processOne con (d, dbi@(DbInsert ct)) (Right l) = do
    newD <- if ct `mod` 1000 == 0
                then do
                        commit con
                        count <- getCount con
                        putStrLn (">" ++ show ct ++ " " ++ show count ++ " " ++ (printf "%0.2f" d))
                        return 0
                else return d
    hFlush stdout
    e <- try (M.time $ insert dbi l con) :: IO (Either SqlError (Double, DbInsert))
    case e of
        Left l' -> do
            if (isInfixOf "Data too long" (show l'))
                then do return (newD, DbInsert (ct + 1))
                else do error (show l')
        Right (tt, r) -> return (newD+tt, r)

insert :: IConnection c
       => DbInsert
       -> DataLine
       -> c
       -> IO DbInsert
insert (DbInsert ct) (Message time typ name msg) con = do
    t <- getDate con
    let newT = setHoursMinutes t time
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
    prevName <- getRepName con

    if (prevName /= Just name)
        then do count <- fromMaybe (return 0) ((\p -> getRepCount con p) <$> prevName)
                if (count > 5)
                    then do quickQuery con qs [toSql prevName, toSql count]
                            return ()
                    else return ()
        else updateRep con name <* return ()
    --case (prevName, newRep) of
    --    (Just n, 1) | repCt > 5 -> do seqQ <- prepare con qs
    --                                  force <$> execute seqQ [toSql n, toSql repCt]
    --    _ -> return 1


    let qa = "INSERT INTO allmsgs (hash, contents, repcount, length, hasURL, isComplex)\
            \ VALUES (CRC32(?), ?, 1, ?, ? LIKE '%http://%', ? NOT LIKE '%http://%' AND ? > 12)\
            \ ON DUPLICATE KEY UPDATE repcount=repcount+1;"

    let len = toSql $ length msg
    msgQ <- prepare con qa
    force <$> execute msgQ [sqlMsg, sqlMsg, len, sqlMsg, sqlMsg, len]

    let qact = "INSERT INTO activeusers (name, lastspoke)\
              \ VALUES (?,?)\
              \ ON DUPLICATE KEY UPDATE\
              \ name=name,\
              \ lastspoke=?"
    activeQ <- prepare con qact
    force <$> execute activeQ  [sqlName, sqlTime, sqlTime]

    let qoact = "INSERT INTO activity (dummy,h0,h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)\
               \ VALUES (1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)\
               \ ON DUPLICATE KEY UPDATE\
               \     h0=h0+IF(HOUR(?) = 0, 1, 0),\
               \     h1=h1+IF(HOUR(?) = 1, 1, 0),\
               \     h2=h2+IF(HOUR(?) = 2, 1, 0),\
               \     h3=h3+IF(HOUR(?) = 3, 1, 0),\
               \     h4=h4+IF(HOUR(?) = 4, 1, 0),\
               \     h5=h5+IF(HOUR(?) = 5, 1, 0),\
               \     h6=h6+IF(HOUR(?) = 6, 1, 0),\
               \     h7=h7+IF(HOUR(?) = 7, 1, 0),\
               \     h8=h8+IF(HOUR(?) = 8, 1, 0),\
               \     h9=h9+IF(HOUR(?) = 9, 1, 0),\
               \     h10=h10+IF(HOUR(?) = 10, 1, 0),\
               \     h11=h11+IF(HOUR(?) = 11, 1, 0),\
               \     h12=h12+IF(HOUR(?) = 12, 1, 0),\
               \     h13=h13+IF(HOUR(?) = 13, 1, 0),\
               \     h14=h14+IF(HOUR(?) = 14, 1, 0),\
               \     h15=h15+IF(HOUR(?) = 15, 1, 0),\
               \     h16=h16+IF(HOUR(?) = 16, 1, 0),\
               \     h17=h17+IF(HOUR(?) = 17, 1, 0),\
               \     h18=h18+IF(HOUR(?) = 18, 1, 0),\
               \     h19=h19+IF(HOUR(?) = 19, 1, 0),\
               \     h20=h20+IF(HOUR(?) = 20, 1, 0),\
               \     h21=h21+IF(HOUR(?) = 21, 1, 0),\
               \     h22=h22+IF(HOUR(?) = 22, 1, 0),\
               \     h23=h23+IF(HOUR(?) = 23, 1, 0);"
    overallActiveQ <- prepare con qoact
    force <$> execute overallActiveQ [sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime, sqlTime]
    let qq = "INSERT INTO users (name, msgcount, wordcount, charcount, lastseen, firstseen, isExclamation, isQuestion, isAmaze, isTxt, isNaysay, isApostrophe, isCaps, isWelcoming, q1, q2, q3, q4, timesMentioned, timesMentioning) \
            \ VALUES (?, 1, 0, 0, ?, ?, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)\
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
            \     isAmaze=isAmaze+(IF(? LIKE '%wow%'\
            \                     AND ? REGEXP '[[:<:]]wow[[:>:]]|really.?$', 1, 0)),\
            \     isTxt=isTxt+(IF(? REGEXP '[[:<:]](wat|wot|r|u|k|idk|ikr|v)[[:>:]]', 1, 0)),\
            \     isNaysay=isNaysay+(IF(? LIKE '%no%'\
            \                       AND ? REGEXP '[[:<:]]no[[:>:]]', 1, 0)),\
            \     isApostrophe=isApostrophe+(IF(? LIKE '%''%', 1, 0)),\
            \     isCaps=isCaps+(IF(? = BINARY UPPER(?), 1, 0)),\
            \     isWelcoming=isWelcoming+(IF(? REGEXP '[[:<:]](welcome|hi|hello|good morning)[[:>:]]', 1, 0)),\
            \     q1=q1+(IF(FLOOR(HOUR(?)/6) = 0, 1, 0)),\
            \     q2=q2+(IF(FLOOR(HOUR(?)/6) = 1, 1, 0)),\
            \     q3=q3+(IF(FLOOR(HOUR(?)/6) = 2, 1, 0)),\
            \     q4=q4+(IF(FLOOR(HOUR(?)/6) = 3, 1, 0))"

    countQ <- prepare con qq
    force <$> execute countQ [ sqlName, sqlTime, sqlTime
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
                             , sqlMsg
                             , sqlTime
                             , sqlTime
                             , sqlTime
                             , sqlTime
                             ]

    let qt1 = "INSERT INTO totals (dummy, msgcount, wordcount, startDate, endDate)\
             \ VALUES (1,1,?,?,?)\
             \ ON DUPLICATE KEY UPDATE endDate=?"
    totalsQ1 <- prepare con qt1
    force <$> execute totalsQ1 [wordcount, sqlTime, sqlTime, sqlTime]

    let qt2 = "UPDATE totals\
             \ SET wordcount=wordcount+?,\
             \     msgcount=msgcount+1"
    totalsQ2 <- prepare con qt2
    force <$> execute totalsQ2 [wordcount]

    let qurl = "INSERT INTO urls (name, contents)\
              \ (SELECT ?, ?\
              \  FROM DUAL\
              \  WHERE ? LIKE '%http://%'\
              \ LIMIT 1);"
    urlQ <- prepare con qurl
    force <$> execute urlQ [sqlName, sqlMsg, sqlMsg]

    let qm = "INSERT INTO messages (name, type, userindex, wordcount, charcount, contents, contentspre, time, hour, quartile, hash)\
            \ VALUES (?,?,\
            \         IFNULL((SELECT msgcount FROM users WHERE name=?), 0),\
            \         ?,?,?,?,?,\
            \         HOUR(?),\
            \         HOUR(?)/6,\
            \         CRC32(?));"
    message <- prepare con qm
    force <$> execute message [ sqlName, sqlType
                              , sqlName
                              , wordcount, charcount, sqlMsg, sqlPre, sqlTime
                              , sqlTime
                              , sqlTime
                              , sqlMsg]


    let qp = "INSERT IGNORE INTO mentions (mentioner, mentionee, num)\
            \ (SELECT ?, name, 0 FROM activeusers)"
    mention <- prepare con qp
    force <$> execute mention [sqlName]

    let qMention = "UPDATE users AS mentioner\
                  \ JOIN (SELECT \
                  \           name AS m,\
                  \           (? LIKE CONCAT('%', name, '%')\
                  \            AND ? REGEXP CONCAT('[[:<:]]',\
                  \                                name,\
                  \                                '[[:>:]]')) AS mmatch\
                  \       FROM activeusers) AS c1\
                  \ SET \
                  \     timesMentioned=timesMentioned+IF(mentioner.name=m, 1, 0),\
                  \     timesMentioning=timesMentioning+IF(mentioner.name=?, 1, 0)\
                  \ WHERE\
                  \     mmatch AND (mentioner.name = m OR mentioner.name = ?)"
    mentionQ <- prepare con qMention
    force <$> execute mentionQ [sqlMsg, sqlMsg, sqlName, sqlName, sqlName, sqlName, sqlName]


    let qdel = "DELETE FROM activeusers\
              \ WHERE LENGTH(name) < 3 OR DATEDIFF(?, lastspoke) >= 5"
    deleteQ <- prepare con qdel

    force <$> execute deleteQ [sqlTime]

    let qqp = "UPDATE mentions\
             \ JOIN (SELECT * FROM activeusers) AS u\
             \ SET num=num+IF(? LIKE CONCAT('%', u.name, '%')\
             \                AND ? REGEXP CONCAT('[[:<:]]',\
             \                                    REPLACE(u.name, '|', '\\|'),\
             \                                    '[[:>:]]'), 1, 0)\
             \ WHERE mentioner=? AND mentionee = u.name\
             \                   AND mentioner != mentionee"
    mention2 <- prepare con qqp
    force <$> execute mention2 [sqlMsg, sqlMsg, sqlName]
    updateDate con newT
    return (DbInsert (ct+1))
insert (DbInsert ct) (Nick time old new) con = do
    t <- getDate con
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO nickchanges (oldname, newname, time)\
                           \ VALUES (?,?,?);"
    let sqlOld = toSql old
    let sqlMsg = toSql new
    let sqlTime = toSql newT
    force <$> execute prepared [sqlOld, sqlMsg, sqlTime]
    updateDate con newT
    return (DbInsert (ct+1))
insert (DbInsert ct) (Kick time kickee kicker reason) con = do
    t <- getDate con
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO kicks (kicker, kickee, reason, time)\
                           \ VALUES (?,?,?, ?);"
    let sqlKicker = toSql kicker
    let sqlKickee = toSql kickee
    let sqlReason = toSql reason
    let sqlTime = toSql newT
    force <$> execute prepared [sqlKicker, sqlKickee, sqlReason, sqlTime]
    updateDate con newT
    return (DbInsert (ct+1))
insert (DbInsert ct) (Topic time setter topic) con = do
    t <- getDate con
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO topics (name, topic, time)\
                           \ VALUES (?,?,?);"
    let sqlName = toSql setter
    let sqlTopic = toSql topic
    let sqlTime = toSql newT
    force <$> execute prepared [sqlName, sqlTopic, sqlTime]
    updateDate con newT
    return (DbInsert (ct+1))
insert (DbInsert ct) (Join time name) con = do
    t <- getDate con
    let newT = setHoursMinutes t time
    prepared <- prepare con "INSERT INTO joins (name, num)\
                           \ VALUES (?, 1)\
                           \ ON DUPLICATE KEY UPDATE\
                           \     num = num+1"
    let sqlName = toSql name
    force <$> execute prepared [sqlName]
    updateDate con newT
    return (DbInsert (ct+1))
insert (DbInsert ct) (Day date) con = do
    updateDate con date
    return (DbInsert (ct+1))
insert (DbInsert ct) (Open date) con = do
    updateDate con date
    return (DbInsert (ct+1))
insert (DbInsert ct) _ _ =
    return (DbInsert (ct+1))

updateDate :: IConnection c => c -> LocalTime -> IO ()
updateDate con date = do
    let sqlDate = toSql date
    quickQuery con "INSERT INTO savedate (date) VALUES (?)\
                  \ ON DUPLICATE KEY UPDATE date=?" [sqlDate, sqlDate]
    return ()

getDate :: IConnection c => c -> IO LocalTime
getDate con = do
    val <- quickQuery con "SELECT date\
                         \ FROM savedate\
                         \ LIMIT 1" []
    let extract ((x:_):_) = fromSql x
        extract _         = anyTime
    return $ extract val

updateRep :: IConnection c => c -> String -> IO ()
updateRep con s = do
    let sqlName = toSql s
    quickQuery con "INSERT INTO repuser (name, num)\
                  \ VALUES (?, 0)\
                  \ ON DUPLICATE KEY UPDATE \
                  \     name=?,\
                  \     num=1" [sqlName, sqlName]
    return ()

getRepCount :: IConnection c => c -> String -> IO Int
getRepCount con s = do
    let sqlName = toSql s
    val <- quickQuery con "IFNULL(SELECT num \
                      \        FROM repuser \
                      \        WHERE name=? \
                      \        LIMIT 1, \
                      \        0)" [sqlName]
    let extract ((x:_):_) = fromSql x :: Int
        extract [] = 0
    return $ extract val

getRepName :: IConnection c => c -> IO (Maybe String)
getRepName con = do
    val <- quickQuery con "SELECT name FROM repuser" []
    let extract ((x:_):_) = Just $ fromSql x :: Maybe String
        extract [] = Nothing
    return $ extract val

deleteTemps :: IConnection c => c -> IO ()
deleteTemps con = do
    runQuery con "TRUNCATE uniquenicks;"
    runQuery con "TRUNCATE top;"
    return ()

populateTop :: IConnection c => c -> IO ()
populateTop con = do
    runQuery con "INSERT INTO top (name, msgcount)\
                 \ (SELECT name, msgcount\
                 \  FROM users\
                 \  ORDER BY msgcount DESC\
                 \  LIMIT 10);"
    return ()

-- a nick is "unique" if it has over N messages and doesnt have an oldnick such that
-- numMessages(oldNick) => numMessages(nick)
populateUnique :: IConnection c => c -> IO ()
populateUnique con = do
    let q = "INSERT INTO uniquenicks (name, msgcount)\
           \ (SELECT activeusers.name, users.msgcount\
           \  FROM activeusers\
           \  INNER JOIN users\
           \  ON users.name = activeusers.name)"
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
                                 , "DROP TABLE IF EXISTS users;"
                                 , "DROP TABLE IF EXISTS uniquenicks;"
                                 , "DROP TABLE IF EXISTS mentions;"
                                 , "DROP TABLE IF EXISTS allusers;"
                                 , "DROP TABLE IF EXISTS allmsgs;"
                                 , "DROP TABLE IF EXISTS seqcount;"
                                 , "DROP TABLE IF EXISTS urls;"
                                 , "DROP TABLE IF EXISTS totals;"
                                 , "DROP TABLE IF EXISTS activeusers;"
                                 , "DROP TABLE IF EXISTS joins;"
                                 , "DROP TABLE IF EXISTS activity;"
                                 , "DROP TABLE IF EXISTS savedate;"
                                 , "DROP TABLE IF EXISTS repuser;"
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
                                        \ name CHAR(21) NOT NULL,\
                                        \ time DATETIME NOT NULL,\
                                        \ hour TINYINT UNSIGNED NOT NULL,\
                                        \ quartile TINYINT UNSIGNED NOT NULL,\
                                        \ hash INT UNSIGNED NOT NULL,\
                                        \ PRIMARY KEY (id),\
                                        \ KEY (hash),\
                                        \ INDEX(userindex))\
                  \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    let seqcount = "CREATE TABLE seqcount(id INT NOT NULL AUTO_INCREMENT,\
                                        \ name CHAR(21) NOT NULL,\
                                        \ num INT NOT NULL,\
                                        \ PRIMARY KEY (id));"
    let statuses = "CREATE TABLE statuses(id INT NOT NULL AUTO_INCREMENT,\
                                        \ contents VARCHAR(500) NOT NULL,\
                                        \ name CHAR(21) NOT NULL,\
                                        \ time DATETIME NOT NULL,\
                                        \ PRIMARY KEY (id))\
                  \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    let nickchanges = "CREATE TABLE nickchanges(id INT NOT NULL AUTO_INCREMENT,\
                                              \ oldname CHAR(21) NOT NULL,\
                                              \ newname CHAR(21) NOT NULL,\
                                              \ time DATETIME NOT NULL,\
                                              \ PRIMARY KEY (id));"
    let topics = "CREATE TABLE topics(id INT NOT NULL AUTO_INCREMENT,\
                                    \ name CHAR(21) NOT NULL,\
                                    \ topic VARCHAR(500) NOT NULL,\
                                    \ time DATETIME NOT NULL,\
                                    \ PRIMARY KEY (id))\
                \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    let kicks = "CREATE TABLE kicks(id INT NOT NULL AUTO_INCREMENT,\
                                  \ kicker CHAR(21) NOT NULL,\
                                  \ kickee CHAR(21) NOT NULL,\
                                  \ reason VARCHAR(500) NOT NULL,\
                                  \ time DATETIME NOT NULL,\
                                  \ PRIMARY KEY (id))\
               \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    let joins = "CREATE TABLE joins(name CHAR(21) NOT NULL,\
                                  \ num MEDIUMINT UNSIGNED NOT NULL,\
                                  \ PRIMARY KEY (name));"
    let top = "CREATE TABLE top(id INT NOT NULL AUTO_INCREMENT,\
                              \ name CHAR(21) NOT NULL,\
                              \ msgcount INT NOT NULL,\
                              \ PRIMARY KEY (id))\
             \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"
    let activity = "CREATE TABLE activity(dummy BOOL NOT NULL,\
                                        \ h0 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h1 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h2 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h3 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h4 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h5 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h6 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h7 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h8 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h9 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h10 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h11 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h12 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h13 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h14 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h15 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h16 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h17 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h18 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h19 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h20 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h21 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h22 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ h23 MEDIUMINT UNSIGNED NOT NULL,\
                                        \ PRIMARY KEY (dummy));"
    let count = "CREATE TABLE users(name CHAR(21) NOT NULL,\
                                  \ msgcount MEDIUMINT UNSIGNED NOT NULL,\
                                  \ wordcount MEDIUMINT UNSIGNED NOT NULL,\
                                  \ charcount MEDIUMINT UNSIGNED NOT NULL,\
                                  \ lastseen DATETIME NOT NULL,\
                                  \ firstseen DATETIME NOT NULL,\
                                  \ timesMentioned MEDIUMINT UNSIGNED NOT NULL,\
                                  \ timesMentioning MEDIUMINT UNSIGNED NOT NULL,\
                                  \ isExclamation MEDIUMINT UNSIGNED NOT NULL,\
                                  \ isQuestion MEDIUMINT UNSIGNED NOT NULL,\
                                  \ isAmaze MEDIUMINT UNSIGNED NOT NULL,\
                                  \ isTxt MEDIUMINT UNSIGNED NOT NULL,\
                                  \ isNaysay MEDIUMINT UNSIGNED NOT NULL,\
                                  \ isApostrophe MEDIUMINT UNSIGNED NOT NULL,\
                                  \ isCaps MEDIUMINT UNSIGNED NOT NULL,\
                                  \ isWelcoming MEDIUMINT UNSIGNED NOT NULL,\
                                  \ q1 MEDIUMINT UNSIGNED NOT NULL,\
                                  \ q2 MEDIUMINT UNSIGNED NOT NULL,\
                                  \ q3 MEDIUMINT UNSIGNED NOT NULL,\
                                  \ q4 MEDIUMINT UNSIGNED NOT NULL,\
                                  \ PRIMARY KEY (name));"
    let unique = "CREATE TABLE uniquenicks(id INT NOT NULL AUTO_INCREMENT,\
                                         \ name CHAR(21) NOT NULL,\
                                         \ msgcount INT NOT NULL,\
                                         \ PRIMARY KEY (id));"
    let activeusers = "CREATE TABLE activeusers(name CHAR(21) NOT NULL,\
                                              \ lastspoke DATETIME NOT NULL,\
                                              \ PRIMARY KEY (name));"
    let allmsgs = "CREATE TABLE allmsgs(contents VARCHAR(500) NOT NULL,\
                                      \ repcount INT NOT NULL,\
                                      \ length INT NOT NULL,\
                                      \ hasURL BOOL NOT NULL,\
                                      \ isComplex BOOL NOT NULL,\
                                      \ hash CHAR(50) NOT NULL,\
                                      \ PRIMARY KEY (hash),\
                                      \ INDEX (repcount))\
                 \ CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;"

    let mentions = "CREATE TABLE mentions(mentioner CHAR(21) NOT NULL,\
                                        \ mentionee CHAR(21) NOT NULL,\
                                        \ num INT NOT NULL,\
                                        \ PRIMARY KEY (mentioner, mentionee));"
    let urls = "CREATE TABLE urls(id INT NOT NULL AUTO_INCREMENT,\
                                \ name CHAR(21) NOT NULL,\
                                \ contents VARCHAR(500) NOT NULL,\
                                \ PRIMARY KEY (id));"
    let totals = "CREATE TABLE totals(dummy BOOL NOT NULL,\
                                    \ wordcount INT NOT NULL,\
                                    \ msgcount INT NOT NULL,\
                                    \ startDate DATETIME NOT NULL,\
                                    \ endDate DATETIME NOT NULL,\
                                    \ PRIMARY KEY (dummy));"
    let savedate = "CREATE TABLE savedate(date DATETIME NOT NULL,\
                                       \ PRIMARY KEY (date));"
    let repuser = "CREATE TABLE repuser(name CHAR(21) NOT NULL,\
                                      \ num INT NOT NULL,\
                                      \ PRIMARY KEY (name));"
    sequence_ $ runQuery con <$> [ messages
                                 , statuses
                                 , nickchanges
                                 , topics
                                 , kicks
                                 , top
                                 , count
                                 , unique
                                 , mentions
                                 , allmsgs
                                 , seqcount
                                 , urls
                                 , totals
                                 , activeusers
                                 , joins
                                 , activity
                                 , savedate
                                 , repuser
                                 ]
    return ()

--doRead :: IConnection c => c -> IO ()
--doRead con =
--    line <- readLn :: IO String
--    let parsed = parseLine line
--    case parsed of
--        Left err ->
--        Right line -> insert

populateDbs :: IConnection c => c -> IO ()
populateDbs con = do
    logfile <- readConfig
    bytestring <- readFile logfile
    let utf8' = unpack $ decodeUtf8 bytestring
    let contents = lines utf8'
    let parsed = parseLine <$> zip [1..] contents
    let seed = (0, DbInsert 0)
    foldlM (processOne con) seed parsed
    commit con

repopulateDb :: IConnection c => c -> IO ()
repopulateDb con = do
    deleteDbs con
    createDbs con
    populateDbs con
