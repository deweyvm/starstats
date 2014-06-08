{-# LANGUAGE DoAndIfThenElse #-}
module StarStats.DB.Queries where

import Control.Arrow(second)
import Control.Applicative
import Database.HDBC
import Data.List (sortBy)
import Text.Printf
import StarStats.DB.Utils
import System.IO

getUniqueNicks :: IConnection c => c -> IO [(String,Int)]
getUniqueNicks con =
    let q = "SELECT name, msgcount\
           \ FROM uniquenicks\
           \ ORDER BY msgcount DESC;" in
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
    return $ zip (show <$> [0..]) (concat $ extract <$> times)

getDailyActivity :: IConnection c => c -> IO [(String,Int)]
getDailyActivity con = do
    let q = "SELECT d0, d1, d2, d3, d4, d5, d6\
           \ FROM activity;"
    let extract (d0:d1:d2:d3:d4:d5:d6:_) = [fromSql d0, fromSql d1, fromSql d2, fromSql d3, fromSql d4, fromSql d5, fromSql d6]
    times <- runQuery con q
    return $ zip (["S", "M", "T", "W", "T", "F", "S"]) (concat $ extract <$> times)

getMonthlyActivity :: IConnection c => c -> IO [(String, Int)]
getMonthlyActivity con = do
    let q = "SELECT mon, num\
           \ FROM monthly\
           \ ORDER BY monthyear\
           \ LIMIT 12"
    vals <- runQuery con q
    let extract (x:y:_) = (fromSql x, fromSql y)
    if length vals == 0
    then return []
    else return $ take 12 ((extract <$> vals) ++ (zip (repeat "") (repeat 0)))




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

getRandTopTen :: IConnection c => c -> IO [(String, String)]
getRandTopTen con = do

    let q = "SELECT m.name, contents, m.type\
           \ FROM messages AS m\
           \ JOIN (SELECT \
           \           FLOOR(RAND() * msgcount) AS r, \
           \           name, \
           \           msgcount\
           \       FROM top) AS t\
           \ ON m.name = t.name AND m.userindex = r"

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


getAverageWordCount :: IConnection c => c -> IO [(String, Double)]
getAverageWordCount con =
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

getWelcomers :: IConnection c => c -> IO [(String, Int)]
getWelcomers con =
    let q = "SELECT name, isWelcoming AS c\
           \ FROM users\
           \ WHERE isWelcoming > 0 \
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getIdlers :: IConnection c => c -> IO [(String,Double)]
getIdlers con =
    let q = "SELECT joins.name, IFNULL(num/c.msgcount, '1e500') as c\
           \ FROM joins \
           \ JOIN users as c \
           \ ON c.name = joins.name\
           \ WHERE num > c.msgcount/10 AND num > 10\
           \ ORDER BY c DESC\
           \ LIMIT 10" in
    getAndExtract con [] extractTup q

getNaysayers :: IConnection c => c -> IO [(String,Double)]
getNaysayers con =
    let q = "SELECT m.name, IFNULL(isNaysay/m.msgcount, 0) as c\
           \ FROM users as m\
           \ JOIN uniquenicks as u\
           \ ON u.name = m.name\
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
           \ WHERE timesMentioned > 0\
           \ ORDER BY c DESC\
           \ LIMIT 10;"  in
    getAndExtract con [] extractTup q

getNeedy :: IConnection c => c -> IO [(String, String)]
getNeedy con =
    let q = "SELECT users.name, timesMentioning AS c\
           \ FROM users\
           \ JOIN uniquenicks AS u\
           \ ON u.name = users.name\
           \ WHERE timesMentioning > 0\
           \ ORDER BY c DESC\
           \ LIMIT 10;"  in
    getAndExtract con [] extractTup q

getRepeatedSimple :: IConnection c => c -> IO [(String, Int)]
getRepeatedSimple con =
    let q = "SELECT contents, repcount\
           \ FROM allmsgs\
           \ WHERE repcount > 1\
           \ ORDER BY repcount DESC\
           \ LIMIT 5;" in
    getAndExtract con [] extractTup q

getRepeatedComplex :: IConnection c => c -> IO [(String, Int)]
getRepeatedComplex con = do
    let q = "SELECT contents, repcount\
           \ FROM allmsgs\
           \ WHERE isComplex AND repcount > 1\
           \ ORDER BY repcount DESC\
           \ LIMIT 10;"
    getAndExtract con [] (mapFst escapeHtml . extractTup) q

getTextSpeakers :: IConnection c => c -> IO [(String, Int)]
getTextSpeakers con =
    let q = "SELECT name, isTxt AS c\
           \ FROM users\
           \ WHERE isTxt > 0\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getApostrophes :: IConnection c => c -> IO [(String,String)]
getApostrophes con = do
    let q1 = "SELECT users.name, IFNULL(100*(isApostrophe/users.msgcount), 0) AS c\
            \ FROM users\
            \ JOIN uniquenicks\
            \ ON uniquenicks.name = users.name AND users.msgcount > 100\
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

getQuestions :: IConnection c => c -> IO [(String, Int)]
getQuestions con =
    let q = "SELECT name, isQuestion AS c\
           \ FROM users\
           \ WHERE isQuestion > 0\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getAmazed :: IConnection c => c -> IO [(String,Int)]
getAmazed con =
    let q = "SELECT name, isAmaze as c\
           \ FROM users\
           \ WHERE isAmaze > 0\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getExcited :: IConnection c => c -> IO [(String, Int)]
getExcited con =
    let q = "SELECT name, isExclamation as c\
           \ FROM users\
           \ WHERE isExclamation > 0\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getYell :: IConnection c => c -> IO [(String, Int)]
getYell con =
    let q = "SELECT name, isCaps AS c\
           \ FROM users\
           \ WHERE isCaps > 0\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getWellSpoken :: IConnection c => c -> IO [(String, Double)]
getWellSpoken con =
    let q = "SELECT \
           \     name,\
           \     IFNULL(wordcount/msgcount + charcount/wordcount, 0) as c\
           \ FROM users\
           \ HAVING c > 0\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
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
