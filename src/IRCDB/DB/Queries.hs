module IRCDB.DB.Queries where

import Control.Arrow(second)
import Control.Applicative
import Database.HDBC

import IRCDB.DB.Utils
import IRCDB.Renderer

getUniqueNicks :: IConnection c => c -> IO [(String,Int)]
getUniqueNicks con =
    let q = "SELECT name, count\
           \ FROM uniquenicks\
           \ ORDER BY count DESC" in
    getAndExtract con [] extractTup q

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
    getAndExtract con qs extractTup q

getRandTopTen :: IConnection c => c -> IO [(String, String)]
getRandTopTen con = do
    let q = "SELECT m.name, text\
           \ FROM messages AS m\
           \ INNER JOIN (SELECT ROUND(RAND() * msgs) AS r, name, msgs\
                       \ FROM top) AS t\
           \ ON m.name = t.name AND m.userindex = r"

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

getUsers :: IConnection c => c -> IO [(String, Int)]
getUsers con =
    let q = "SELECT name, msgs FROM top" in
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
    prepared <- prepare con "SELECT DISTINCT name, text\
                           \ FROM messages\
                           \ WHERE text REGEXP ?\
                           \ ORDER BY RAND()\
                           \ LIMIT 10"
    execute prepared [toSql urlRegexp]
    rows <- fetchAllRows' prepared
    let r = (second (linkify.extractUrl)) <$> extractTup <$> rows
    return r


getAverageWordCount :: IConnection c => c -> IO [(String, Double)]
getAverageWordCount con =
    let q = "SELECT m.name, AVG(m.wordcount) AS avg\
           \ FROM top AS t\
           \ INNER JOIN messages AS m\
           \ ON t.name = m.name\
           \ GROUP BY m.name\
           \ ORDER BY avg DESC" in
    getAndExtract con [] extractTup q

getAverageWordLength :: IConnection c => c -> IO [(String, Double)]
getAverageWordLength con =
    let q = "SELECT m.name, IFNULL(SUM(m.charcount)/SUM(m.wordcount), 0) AS avg\
           \ FROM top AS t\
           \ INNER JOIN messages AS m\
           \ ON t.name = m.name\
           \ GROUP BY m.name\
           \ ORDER BY avg DESC" in
    getAndExtract con [] extractTup q

getTimes :: IConnection c
         => c
         -> IO [(String, Int, Int, Int, Int)]
getTimes con = do
    let all' = "SELECT messages.name, FLOOR(HOUR(time)/6) AS h, COUNT(*) AS count\
              \ FROM messages\
              \ JOIN top AS t\
              \ ON t.name = messages.name\
              \ GROUP BY h, messages.name\
              \ ORDER BY messages.name, h, count DESC;"
    let extract (x:y:z:_) = (fromSql x, fromSql y, fromSql z) :: (String, Int, Int)
    xs <- getAndExtract con [] extract all'
    return $ (assemble2 . assemble) xs

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
               \ name, MAX(c) AS maxc\
           \ FROM (\
               \ SELECT\
                  \ name,\
                  \ IF (@name = name,  @count := @count + 1, @count := 1) AS c,\
                  \ @name := name\
               \ FROM\
                  \ messages\
               \ JOIN (\
                   \ SELECT\
                      \ @name:=\"\",\
                      \ @count:=0\
               \ ) AS r\
           \ ) AS t\
           \ WHERE c > 5\
           \ GROUP BY name\
           \ ORDER BY maxc DESC\
           \ LIMIT 10" in
    getAndExtract con [] extractTup q

mostMentions :: IConnection c => c -> IO [(String, String)]
mostMentions con =
    let q = "SELECT u.name, COUNT(*) AS c\
           \ FROM messages\
           \ INNER JOIN (SELECT uniquenicks.name, CONCAT(\"%\", uniquenicks.name, \"%\") AS nn\
                       \ FROM uniquenicks) AS u\
           \ WHERE messages.text LIKE nn\
           \ GROUP BY u.name\
           \ ORDER BY c DESC\
           \ LIMIT 10"  in
    getAndExtract con [] extractTup q


getBffs :: IConnection c => c -> IO [(String, String)]
getBffs con = do
    let qs = "DROP FUNCTION IF EXISTS countmentions;\
            \ DELIMITER $$\
            \ CREATE FUNCTION countmentions (n1 VARCHAR(36), n2 VARCHAR(36))\
            \ RETURNS INT\
            \ BEGIN\
            \ RETURN (SELECT COUNT(*) AS c\
                    \ FROM messages\
                    \ INNER JOIN (SELECT CONCAT(\"%\", n2, \"%\") AS nn) AS k\
                    \ WHERE messages.name = n1 AND messages.text LIKE nn);\
            \ END$$"
    let q = "SELECT \
               \ u.name, \
               \ v.name, \
               \ countmentions(u.name, v.name) AS c1, \
               \ countmentions(v.name, u.name) AS c2\
           \ FROM uniquenicks AS u\
           \ INNER JOIN uniquenicks AS v\
           \ ON u.id < v.id\
           \ HAVING c1 > 100 OR c2 > 100;"
    executeRaw <$> (prepare con qs)
    let extract :: [SqlValue] -> [(String, String)]
        extract (w:x:y:z:_) = [ (fromSql w ++ " mentioned " ++ fromSql x, fromSql y)
                              , (fromSql x ++ " mentioned " ++ fromSql w, fromSql z)]
    concat <$> getAndExtract con [] extract q

getNaysayers :: IConnection c => c -> IO [(String,Double)]
getNaysayers con =
    let q = "SELECT m.name, COUNT(*)/cc as c\
           \ FROM messages as m\
           \ JOIN (SELECT name, COUNT(*) as cc FROM messages GROUP BY name) as j\
           \ ON j.name = m.name\
           \ JOIN uniquenicks as u ON u.name = j.name\
           \ WHERE text REGEXP '[[:<:]]no[[:>:]]' \
           \ GROUP BY m.name\
           \ ORDER BY c DESC\
           \ LIMIT 10" in

    getAndExtract con [] (mapSnd (*100) . extractTup) q

mostNeedy :: IConnection c => c -> IO [(String, String)]
mostNeedy con =
    let q = "SELECT messages.name, COUNT(*) AS c\
           \ FROM messages\
           \ INNER JOIN (SELECT uniquenicks.name, CONCAT(\"%\", uniquenicks.name, \"%\") AS nn\
                       \ FROM uniquenicks) AS u\
           \ WHERE messages.text LIKE nn\
           \ GROUP BY messages.name\
           \ ORDER BY c DESC\
           \ LIMIT 10"  in
    getAndExtract con [] extractTup q

getQuestions :: IConnection c => c -> IO [(String, Int)]
getQuestions con =
    let q = "SELECT name, COUNT(*) AS c\
           \ FROM messages\
           \ WHERE text LIKE '%?'\
           \ GROUP BY name\
           \ ORDER BY c DESC\
           \ LIMIT 10" in
    getAndExtract con [] extractTup q

getRepeatedSimple :: IConnection c => c -> IO [(String, Int)]
getRepeatedSimple con =
    let q = "SELECT text, COUNT(*) AS c\
           \ FROM messages\
           \ GROUP BY text\
           \ ORDER BY c DESC\
           \ LIMIT 5" in
    getAndExtract con [] extractTup q

getRepeatedComplex :: IConnection c => c -> IO [(String, Int)]
getRepeatedComplex con = do
    let q = "SELECT text, COUNT(*) AS c\
           \ FROM messages\
           \ WHERE CHAR_LENGTH(text) > 12 AND NOT text LIKE '%http%'\
           \ GROUP BY text\
           \ HAVING c > 10\
           \ ORDER BY c DESC"
    elts <- getAndExtract con [] (mapFst escapeHtml . extractTup) q
    return $ filter (\(x, _) -> length x > 12) elts

