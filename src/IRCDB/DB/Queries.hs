module IRCDB.DB.Queries where

import Control.Arrow(second)
import Control.Applicative
import Database.HDBC
import Data.List (sortBy)
import Text.Printf
import IRCDB.DB.Utils

getUniqueNicks :: IConnection c => c -> IO [(String,Int)]
getUniqueNicks con =
    let q = "SELECT name, count\
           \ FROM uniquenicks\
           \ ORDER BY count DESC;" in
    getAndExtract con [] extractTup q

getOverallActivity :: IConnection c => c -> IO [(Int,Int)]
getOverallActivity con = do
    let q = "SELECT h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19, h20, h21, h22, h23\
           \ FROM activity;"
    let extract (_:y:_) = fromSql y
    times <- runQuery con q
    return $ zip [0..] (extract <$> times)

getRandMessages :: IConnection c => c -> IO [(String, String)]
getRandMessages con =
    let qs = ["SET @max = (SELECT MAX(id) FROM messages); "] in
    let q = "SELECT name, contents\
           \ FROM messages AS m\
           \ JOIN (SELECT FLOOR(RAND() * @max) AS m2\
                 \ FROM messages\
                 \ LIMIT 10) AS dummy\
           \ ON m.id = m2;" in
    getAndExtract con qs extractTup q

getRandTopTen :: IConnection c => c -> IO [(String, String)]
getRandTopTen con = do
    let q = "SELECT m.name, contents\
           \ FROM messages AS m\
           \ JOIN (SELECT \
           \           FLOOR(RAND() * msgs) AS r, \
           \           name, \
           \           msgs\
           \       FROM top) AS t\
           \ ON m.userindex = r AND m.name = t.name;"

    getAndExtract con [] extractTup q

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
    let q = "SELECT name, msgs FROM top;" in
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
           \       FROM counts) AS m\
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
           \       FROM counts) AS m\
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
    let all' = "(SELECT counts.name, 0, q1 \
              \ FROM counts\
              \ JOIN top as t\
              \ ON t.name = counts.name)\
              \ UNION\
              \(SELECT counts.name, 1, q2 \
              \ FROM counts\
              \ JOIN top as t\
              \ ON t.name = counts.name)\
              \ UNION\
              \(SELECT counts.name, 2, q3 \
              \ FROM counts\
              \ JOIN top as t\
              \ ON t.name = counts.name)\
              \ UNION\
              \(SELECT counts.name, 3, q4 \
              \ FROM counts\
              \ JOIN top as t\
              \ ON t.name = counts.name);"
    let extract (x:y:z:_) = (fromSql x, fromSql y, fromSql z) :: (String, Int, Int)
    xs <- getAndExtract con [] extract all'
    let xs' = sortBy cmp xs
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
           \ FROM counts\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getIdlers :: IConnection c => c -> IO [(String,String)]
getIdlers con =
    let q = "SELECT joins.name, IFNULL(num/msgcount, 'Infinity') as c\
           \ FROM joins \
           \ JOIN counts as c \
           \ ON c.name = joins.name\
           \ WHERE num > msgcount/10 AND num > 10\
           \ ORDER BY c DESC\
           \ LIMIT 10" in
    getAndExtract con [] extractTup q

getRelationships :: IConnection c => c -> IO [(String, String)]
getRelationships con = do
    let q = "SELECT \
               \ u.name, \
               \ v.name, \
               \ IFNULL((SELECT count FROM mentions WHERE mentioner = u.name AND mentionee = v.name LIMIT 1), 0) AS c1,\
               \ IFNULL((SELECT count FROM mentions WHERE mentioner = v.name AND mentionee = u.name LIMIT 1), 0) AS c2\
           \ FROM uniquenicks AS u\
           \ INNER JOIN uniquenicks AS v\
           \ ON u.id < v.id\
           \ HAVING c1 > 100 OR c2 > 100\
           \ LIMIT 20;"
    let extract :: [SqlValue] -> [(String, String)]
        extract (w:x:y:z:_) = [ (fromSql w ++ " mentioned " ++ fromSql x, fromSql y)
                              , (fromSql x ++ " mentioned " ++ fromSql w, fromSql z)]
    concat <$> getAndExtract con [] extract q

getNaysayers :: IConnection c => c -> IO [(String,Double)]
getNaysayers con =
    let q = "SELECT m.name, isNaysay/msgcount as c\
           \ FROM counts as m\
           \ JOIN uniquenicks as u\
           \ ON u.name = m.name\
           \ WHERE isNaysay \
           \ GROUP BY m.name\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in

    getAndExtract con [] (mapSnd (*100) . extractTup) q

getPopular :: IConnection c => c -> IO [(String, String)]
getPopular con =
    let q = "SELECT counts.name, timesMentioned AS c\
           \ FROM counts\
           \ JOIN uniquenicks AS u\
           \ ON u.name = counts.name\
           \ ORDER BY c DESC\
           \ LIMIT 10;"  in
    getAndExtract con [] extractTup q

getNeedy :: IConnection c => c -> IO [(String, String)]
getNeedy con =
    let q = "SELECT counts.name, timesMentioning AS c\
           \ FROM counts\
           \ JOIN uniquenicks AS u\
           \ ON u.name = counts.name\
           \ ORDER BY c DESC\
           \ LIMIT 10;"  in
    getAndExtract con [] extractTup q

getRepeatedSimple :: IConnection c => c -> IO [(String, Int)]
getRepeatedSimple con =
    let q = "SELECT contents, count\
           \ FROM allmsgs\
           \ ORDER BY count DESC\
           \ LIMIT 5;" in
    getAndExtract con [] extractTup q

getRepeatedComplex :: IConnection c => c -> IO [(String, Int)]
getRepeatedComplex con = do
    let q = "SELECT contents, count\
           \ FROM allmsgs\
           \ WHERE isComplex AND count > 1\
           \ ORDER BY count DESC\
           \ LIMIT 10;"
    getAndExtract con [] (mapFst escapeHtml . extractTup) q

getTextSpeakers :: IConnection c => c -> IO [(String, Int)]
getTextSpeakers con =
    let q = "SELECT name, isTxt AS c\
           \ FROM counts\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getApostrophes :: IConnection c => c -> IO [(String,String)]
getApostrophes con = do
    let q1 = "SELECT counts.name, 100*(isApostrophe/msgcount) AS c\
            \ FROM counts\
            \ JOIN uniquenicks\
            \ ON uniquenicks.name = counts.name\
            \ ORDER BY c;"

    let showDouble d = printf "%.2f" (d :: Double)
    let extract :: [SqlValue] -> (String, String)
        extract = mapSnd showDouble . extractTup
    r1 <- reverse <$> getAndExtract con []  extract q1
    let (xs, ys) = (getTopBottom 5 r1)
    let res = xs  ++ [("...", "...")] ++ ys
    return $ res

getQuestions :: IConnection c => c -> IO [(String, Int)]
getQuestions con =
    let q = "SELECT name, isQuestion AS c\
           \ FROM counts\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getAmazed :: IConnection c => c -> IO [(String,Int)]
getAmazed con =
    let q = "SELECT name, isAmaze as c\
           \ FROM counts\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getExcited :: IConnection c => c -> IO [(String, Int)]
getExcited con =
    let q = "SELECT name, isExclamation as c\
           \ FROM counts\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getYell :: IConnection c => c -> IO [(String, Int)]
getYell con =
    let q = "SELECT name, isCaps AS c\
           \ FROM counts\
           \ ORDER BY c DESC\
           \ LIMIT 10;" in
    getAndExtract con [] extractTup q

getWellSpoken :: IConnection c => c -> IO [(String, Double)]
getWellSpoken con =
    let q = "SELECT \
           \     name,\
           \     IFNULL(wordcount/msgcount + charcount/wordcount, 0) as c\
           \ FROM counts\
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
