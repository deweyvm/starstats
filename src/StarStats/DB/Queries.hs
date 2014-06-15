{-# LANGUAGE DoAndIfThenElse #-}
module StarStats.DB.Queries where

import Control.Arrow(second)
import Control.Applicative
import Database.HDBC
import Data.List (sortBy)
import Text.Printf
import System.IO
import StarStats.DB.Utils

getUniqueNicks :: IConnection c => c -> IO [(String,Int)]
getUniqueNicks con =
    let q = "SELECT name, msgcount\
           \ FROM uniquenicks\
           \ ORDER BY msgcount DESC\
           \ LIMIT 20;" in
    getAndExtract con [] extractTup q

getHourlyActivity :: IConnection c => c -> IO [(String,Int)]
getHourlyActivity con = do
    let q = "SELECT h0,  h1,  h2,  h3,  h4,  h5,\
           \        h6,  h7,  h8,  h9,  h10, h11,\
           \        h12, h13, h14, h15, h16, h17,\
           \        h18, h19, h20, h21, h22, h23\
           \ FROM activity;"
    let extract (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:x14:x15:x16:x17:x18:x19:x20:x21:x22:x23:_) = [ fromSql x0,  fromSql x1,  fromSql x2,  fromSql x3
                         , fromSql x4,  fromSql x5,  fromSql x6,  fromSql x7
                         , fromSql x8,  fromSql x9,  fromSql x10, fromSql x11
                         , fromSql x12, fromSql x13, fromSql x14, fromSql x15
                         , fromSql x16, fromSql x17, fromSql x18, fromSql x19
                         , fromSql x20, fromSql x21, fromSql x22, fromSql x23]
        extract _ = []
    times <- runQuery con q
    let catted = concat $ extract <$> times
    if length catted < 3
    then return []
    else return $ zip (show <$> [0..]) catted

getDailyActivity :: IConnection c => c -> IO [(String,Int)]
getDailyActivity con = do
    let q = "SELECT d0, d1, d2, d3, d4, d5, d6\
           \ FROM activity;"
    let extract (d0:d1:d2:d3:d4:d5:d6:_) = [fromSql d0, fromSql d1, fromSql d2, fromSql d3, fromSql d4, fromSql d5, fromSql d6]
    times <- runQuery con q
    let catted = concat $ extract <$> times
    if length catted < 3
    then return []
    else return $ zip (["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]) catted

getMonthlyActivity :: IConnection c => c -> IO ([(String, Int)], [(String, Int)])
getMonthlyActivity con = do
    let qmsgs = "SELECT mon, nummsgs\
               \ FROM monthly\
               \ ORDER BY monthyear\
               \ LIMIT 12"
    msgs <- runQuery con qmsgs

    let qusers = "SELECT mon, numusers\
                \ FROM monthly\
                \ ORDER BY monthyear\
                \ LIMIT 12;"
    users <- runQuery con qusers

    let extract (x:y:_) = (fromSql x, fromSql y)
    let order xs = take 12 ((extract <$> xs) ++ (zip (repeat "") (repeat 0)))
    if length msgs < 3
    then return ([],[])
    else return $ (order msgs, order users)




getRandMessages :: IConnection c => c -> IO [(String, String)]
getRandMessages con =
    let qs = ["SET @max = (SELECT MAX(id) FROM messages); "] in
    let q = "SELECT DISTINCT name, contents, m.type\
           \ FROM messages AS m\
           \ JOIN (SELECT FLOOR(RAND() * @max) AS m2\
                 \ FROM messages\
                 \ LIMIT 10) AS dummy\
           \ ON m.id = m2;" in
    getAndExtract con qs extractAction q

getRandTop :: IConnection c => c -> IO [(String, String)]
getRandTop con = do

    let q = "SELECT m.name, contents, m.type\
           \ FROM messages AS m\
           \ JOIN (SELECT \
           \           rand, \
           \           name, \
           \           msgcount\
           \       FROM top LIMIT 20) AS t\
           \ ON m.userindex = t.rand AND m.name = t.name LIMIT 20"

    getAndExtract con [] extractAction q

getKickers :: IConnection c => c -> IO [(String, Int)]
getKickers con =
    let q = "SELECT kicker, COUNT(*) AS count\
           \ FROM kicks\
           \ GROUP BY kicker\
           \ ORDER BY count DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getKickees :: IConnection c => c -> IO [(String, Int)]
getKickees con =
    let q = "SELECT kickee, COUNT(*) AS count\
           \ FROM kicks\
           \ GROUP BY kickee\
           \ ORDER BY count DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getMessageCount :: IConnection c => c -> IO [(String, Int)]
getMessageCount con =
    let q = "SELECT name, msgcount FROM top;" in
    getAndExtract con [] extractTup q

getNicks :: IConnection c => c -> IO [(String, Int)]
getNicks con =
    let q = "SELECT oldname, COUNT(*) AS count\
           \ FROM nickchanges\
           \ GROUP BY oldname\
           \ ORDER BY count DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getUrls :: IConnection c => c -> IO [(String, String)]
getUrls con = do
    let qs = ["SET @max = (SELECT MAX(id) FROM urls);"]
    let q = "SELECT DISTINCT name, contents FROM urls AS v\
           \ JOIN (SELECT ROUND(RAND() * @max) AS r\
                 \ FROM urls\
                 \ LIMIT 10) AS dummy\
           \ ON v.id = r;"
    getAndExtract con qs (second (extractUrl) <$>extractTup) q

getLastSeenTop :: IConnection c => c -> IO [(String,String)]
getLastSeenTop con = do
    let q = "SELECT t.name, DATE(l.time)\
           \ FROM lastseen AS l\
           \ JOIN top AS t\
           \ ON l.name = t.name\
           \ LIMIT 20;"
    getAndExtract con [] extractTup q

getRandomRant :: IConnection c => c -> IO [(String, String)]
getRandomRant con = do
    let qs = ["SET @max = (SELECT MAX(id) FROM rants);"]
    let q = "SELECT name, contents, time\
           \ FROM rants\
           \ JOIN (SELECT ROUND(RAND() * @max) as r FROM DUAL) AS dummy\
           \ ON r = rants.id\
           \ LIMIT 1;"
    getAndExtract con qs extractTup q

getAverageWordsPerLine :: IConnection c => c -> IO [(String, Double)]
getAverageWordsPerLine con =
    let q = "SELECT m.name, wc/mc as avg\
           \ FROM top AS t\
           \ JOIN (SELECT\
           \           name,\
           \           msgcount as mc,\
           \           wordcount as wc\
           \       FROM users) AS m\
           \ ON t.name = m.name\
           \ GROUP BY m.name\
           \ ORDER BY avg DESC;" in
    getAndExtract con [] extractTup q

getAverageWordLength :: IConnection c => c -> IO [(String, Double)]
getAverageWordLength con =
    let q = "SELECT m.name, IFNULL(cc/wc, 0) AS avg\
           \ FROM top AS t\
           \ JOIN (SELECT\
           \           name,\
           \           wordcount as wc,\
           \           charcount as cc\
           \       FROM users) AS m\
           \ ON t.name = m.name\
           \ GROUP BY m.name\
           \ ORDER BY avg DESC;" in
    getAndExtract con [] extractTup q

cmp :: (String, Int, Int) -> (String, Int, Int) -> Ordering
cmp (x0, x1, _) (y0, y1, _)
    | x0 < y0 = GT
    | x0 > y0 = LT
    | otherwise = x1 `compare` y1

getTimes :: IConnection c
         => c
         -> IO [(String, Int, Int, Int, Int)]
getTimes con = do
    let q1 = "SELECT users.name, 0, q1\
            \ FROM users\
            \ JOIN top AS t\
            \ ON t.name = users.name"

    let q2 = "SELECT users.name, 1, q2\
            \ FROM users\
            \ JOIN top AS t\
            \ ON t.name = users.name"

    let q3 = "SELECT users.name, 2, q3\
            \ FROM users\
            \ JOIN top AS t\
            \ ON t.name = users.name"

    let q4 = "SELECT users.name, 3, q4\
            \ FROM users\
            \ JOIN top AS t\
            \ ON t.name = users.name"

    let extract (x:y:z:_) = (fromSql x, fromSql y, fromSql z) :: (String, Int, Int)
    q1s <- getAndExtract con [] extract q1
    q2s <- getAndExtract con [] extract q2
    q3s <- getAndExtract con [] extract q3
    q4s <- getAndExtract con [] extract q4
    let xs' = sortBy cmp (q1s ++ q2s ++ q3s ++ q4s)
    return $ (assemble2 . assemble) xs'

getRandTopics :: IConnection c => c -> IO [(String, String)]
getRandTopics con =
    let qs = ["SET @max = (SELECT MAX(id) FROM topics);"] in
    let q = "SELECT DISTINCT name, topic FROM topics AS v\
           \ JOIN (SELECT ROUND(RAND() * @max) AS r\
                 \ FROM topics\
                 \ LIMIT 10) AS dummy\
           \ ON v.id = r;" in
    getAndExtract con qs extractTup q

getSelfTalk :: IConnection c => c -> IO [(String, Int)]
getSelfTalk con =
    let q = "SELECT\
           \    name, COUNT(*) AS c\
           \ FROM seqcount\
           \ WHERE num > 5\
           \ GROUP BY name\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getFriendly :: IConnection c => c -> IO [(String, Double)]
getFriendly con =
    let q = "SELECT name, IFNULL(100*isFriendly/msgcount, 0) AS c\
           \ FROM users\
           \ WHERE isFriendly > 0 AND msgcount > 10\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getIdlers :: IConnection c => c -> IO [(String,Double)]
getIdlers con =
    let q = "SELECT chantime.name, IFNULL(hours/c.msgcount, 0) as c\
           \ FROM chantime \
           \ JOIN users as c \
           \ ON c.name = chantime.name\
           \ ORDER BY c DESC\
           \ LIMIT 10" in
    getAndExtract con [] extractTup q

getNaysayers :: IConnection c => c -> IO [(String,Double)]
getNaysayers con =
    let q = "SELECT m.name, IFNULL(isNaysay/m.msgcount, 0) as c\
           \ FROM users as m\
           \ JOIN uniquenicks as u\
           \ ON u.name = m.name AND m.msgcount > 10\
           \ WHERE isNaysay \
           \ GROUP BY m.name\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in

    getAndExtract con [] (mapSnd (*100) . extractTup) q

getPopular :: IConnection c => c -> IO [(String, String)]
getPopular con =
    let q = "SELECT users.name, timesMentioned AS c\
           \ FROM users\
           \ JOIN uniquenicks AS u\
           \ ON u.name = users.name\
           \ WHERE timesMentioned > 0 AND users.msgcount > 10\
           \ ORDER BY c DESC\
           \ LIMIT 10;"  in
    getAndExtract con [] extractTup q

getNeedy :: IConnection c => c -> IO [(String, String)]
getNeedy con =
    let q = "SELECT users.name, timesMentioning AS c\
           \ FROM users\
           \ JOIN uniquenicks AS u\
           \ ON u.name = users.name AND u.msgcount > 10\
           \ WHERE timesMentioning > 0\
           \ ORDER BY c DESC\
           \ LIMIT 10;"  in
    getAndExtract con [] extractTup q


getTopUrls :: IConnection c => c -> IO [(String, Int, String, String)]
getTopUrls con = do
    let q = "SELECT DISTINCT url, repcount, saidby, saidwhen\
           \ FROM allurls\
           \ ORDER BY repcount DESC\
           \ LIMIT 10;"
    let extract (w:x:y:z:_) = (fromSql w, fromSql x, fromSql y, fromSql z)
        extract _ = ("error", -1, "", "")
    getAndExtract con [] extract q

getRepeatedSimple :: IConnection c => c -> IO [(String, Int, String, String)]
getRepeatedSimple con =
    let q = "SELECT contents, repcount, saidby, saidwhen\
           \ FROM allmsgs\
           \ WHERE repcount > 1\
           \ ORDER BY repcount DESC\
           \ LIMIT 5;" in
    let extract (w:x:y:z:_) = (fromSql w, fromSql x, fromSql y, fromSql z)
        extract _ = ("error", -1, "", "") in
    getAndExtract con [] extract q


getRepeatedComplex :: IConnection c => c -> IO [(String, Int, String, String)]
getRepeatedComplex con = do
    let q = "SELECT contents, repcount, saidby, saidwhen\
           \ FROM allmsgs\
           \ WHERE isComplex AND repcount > 1\
           \ ORDER BY repcount DESC\
           \ LIMIT 10;"
    let extract (w:x:y:z:_) = (escapeHtml $ fromSql w, fromSql x, fromSql y, fromSql z)
        extract _ = ("error", -1, "", "")
    getAndExtract con [] extract q

getTextSpeakers :: IConnection c => c -> IO [(String, Double)]
getTextSpeakers con =
    let q = "SELECT name, IFNULL(100*isTxt/msgcount, 0) AS c\
           \ FROM users\
           \ WHERE isTxt > 0 AND msgcount > 10\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getApostrophes :: IConnection c => c -> IO [(String,String)]
getApostrophes con = do
    let q1 = "SELECT users.name, IFNULL(100*(isApostrophe/users.msgcount), 0) AS c\
            \ FROM users\
            \ JOIN uniquenicks\
            \ ON uniquenicks.name = users.name AND users.msgcount > 10\
            \ ORDER BY c;"

    let showDouble d = printf "%.2f" (d :: Double)
    let extract :: [SqlValue] -> (String, String)
        extract = mapSnd showDouble . extractTup
    r1 <- reverse <$> getAndExtract con []  extract q1
    if (length r1 == 0)
    then return []
    else do let len = min 5 (length r1 `quot` 2)
            let (xs, ys) = (getTopBottom len r1)
            let res = xs  ++ [("...", "...")] ++ ys
            return $ res

getQuestions :: IConnection c => c -> IO [(String, Double)]
getQuestions con =
    let q = "SELECT name, IFNULL(100*isQuestion/msgcount, 0) AS c\
           \ FROM users\
           \ WHERE isQuestion > 0 AND msgcount > 10\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getAmazed :: IConnection c => c -> IO [(String,Double)]
getAmazed con =
    let q = "SELECT name, IFNULL(100*isAmaze/msgcount, 0) as c\
           \ FROM users\
           \ WHERE isAmaze > 0 AND msgcount > 10\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getExcited :: IConnection c => c -> IO [(String, Double)]
getExcited con =
    let q = "SELECT name, IFNULL(100*isExclamation/msgcount, 0) as c\
           \ FROM users\
           \ WHERE isExclamation > 0 AND msgcount > 10\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getYell :: IConnection c => c -> IO [(String, Double)]
getYell con =
    let q = "SELECT name, IFNULL(100*isCaps/msgcount, 0) AS c\
           \ FROM users\
           \ WHERE isCaps > 0 AND msgcount > 10\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getWellSpoken :: IConnection c => c -> IO [(String, Double)]
getWellSpoken con =
    let q = "SELECT \
           \     u.name,\
           \     IFNULL(wordcount/msgcount + charcount/wordcount, 0) as c\
           \ FROM users AS u\
           \ JOIN activeusers AS a\
           \ ON u.name = a.name AND msgcount > 10\
           \ HAVING c > 0\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getLong :: IConnection c => c -> IO [(String, Double)]
getLong con = do
    let q = "SELECT u.name, IFNULL(100*isLong/msgcount, 0) AS c\
           \ FROM users AS u\
           \ JOIN activeusers AS a\
           \ ON u.name = a.name AND msgcount > 10\
           \ HAVING c > 0\
           \ ORDER BY c DESC\
           \ LIMIT 10;"
    getAndExtract con [] extractTup q

getTotalMessages :: IConnection c => c -> IO Int
getTotalMessages con =
    let q = "SELECT msgcount FROM totals;" in
    getSimple con extractSingle q

getTotalWords :: IConnection c => c -> IO Int
getTotalWords con =
    let q = "SELECT wordcount FROM totals;" in
    getSimple con extractSingle q

getStartDate :: IConnection c => c -> IO String
getStartDate con =
    let q = "SELECT startDate FROM totals;" in
    getSimple con extractSingle q

getEndDate :: IConnection c => c -> IO String
getEndDate con =
    let q = "SELECT endDate FROM totals;" in
    getSimple con extractSingle q

getNow :: IConnection c => c -> IO String
getNow con =
    let q = "SELECT NOW();" in
    getSimple con extractSingle q
