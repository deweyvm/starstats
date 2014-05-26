{-# LANGUAGE DoAndIfThenElse, NoMonomorphismRestriction, BangPatterns, TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}
module IRCDB.DB.Driver where

import Prelude hiding (foldl, concat, sequence_, sum)
import Control.Applicative
import Control.DeepSeq
import Database.HDBC
import Database.HDBC.ODBC
import IRCDB.Renderer
import IRCDB.DB.Utils
import IRCDB.DB.Tables
import IRCDB.DB.Queries

data Action = Repopulate | Generate



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

generate :: IConnection c => c -> IO ()
generate con = do
    populateTop con
    populateUnique con
    commit con
    users <- force <$> getUsers con

    tups <- force <$> getTimes con
    randTop <- force <$> getRandTopTen con
    let bars = (toTimeBars tups)
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
    !self <- getSelfTalk con
    !mentions <- mostMentions con
    !needy <- mostNeedy con
    !questions <- getQuestions con
    !repSimple <- getRepeatedSimple con
    !repComplex <- getRepeatedComplex con
    !nay <- getNaysayers con
    !text <- getTextSpeakers con
    !apos <- getApostrophes con
    -- !bffs <- getBffs con -- expensive
    let printify = (mapSnd print' <$> )
    let col1 = toColumn (printify users) "Messages" 10
    let col2 = toColumn (printify bars) "Active" 10
    let col3 = toColumn (printify avgwl) "AWL" 6
    let col4 = toColumn (printify avgwc) "AWC" 6

    let col5 = toColumn randTop "Random Message" 68

    let us = fst <$> users
    let rows = formatTable us "User" 10 [col1, col2, col3, col4, col5]
    let rendered = unlines $ [ makeTimeScript "Activity (UTC)" activity
                             , withHeading "Top Users" $ rows
                             , headerTable "Apostrophe Users" ("Name", "Percent of Messages with ''s") apos
                             , headerTable "Can't English" ("Name", "Text Speak Count") text
                             , headerTable "Naysayers" ("Name", "Percent Negative") nay
                             , headerTable "Repeated Phrases" ("Phrase", "Times Repeated") repSimple
                             , headerTable "Longer Repeated Phrases" ("Phrase", "Times Repeated") repComplex
                             , headerTable "Clueless" ("Name", "Number Of Questions Asked") questions
                             --, headerTable "Bffs" ("Mention", "Times") bffs
                             , headerTable "Clingy" ("Name", "Times Mentioning Someone") needy
                             , headerTable "Popular" ("Name", "Times Mentioned") mentions
                             , headerTable "Lonely Chatters" ("Name", "Times In A Row") self
                             , headerTable "Unique Nicks" ("Name","Messages") unique
                             , headerTable "Some Random URLs" ("Name", "URL") urls
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
