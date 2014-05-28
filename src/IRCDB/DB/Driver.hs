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
    time' "Populate top" $ populateTop con
    time' "Populate unique" $ populateUnique con
    time' "Commit" $ commit con
    !users <- time' "Get users" $ force <$> getUsers con

    !tups <- time' "Get user activity" $ force <$> getTimes con
    !randTop <- time' "Get random top10" $ force <$> getRandTopTen con
    let bars = (toTimeBars tups)
    !rand <- time' "Random" $ getRandMessages con
    !nicks <- time' "Get nick changes" $ getNicks con
    !kickers <- time' "Get kickers" $ getKickers con
    !kickees <- time' "Get kickees" $ getKickees con
    !topics <- time' "Get random topics" $ getRandTopics con
    !urls <- time' "Get random urls" $ getUrls con
    !activity <- time' "Get overall activity" $ getOverallActivity con
    !unique <- time' "Get unique nicks" $ getUniqueNicks con
    !avgwc <- time' "Get awc" $ getAverageWordCount con
    !avgwl <- time' "Get awl" $ getAverageWordLength con
    !self <- time' "Get self talk" $ getSelfTalk con
    !mentions <- time' "Get mentions" $ mostMentions con
    !needy <- time' "Get needy" $ mostNeedy con
    !questions <- time' "Get questions" $ getQuestions con
    !repSimple <- time' "Get repeated phrases" $ getRepeatedSimple con
    !repComplex <- time' "Get complex rep. phrases" $ getRepeatedComplex con
    !nay <- time' "Get naysayers" $ getNaysayers con
    !text <- time' "Get txt spk" $ getTextSpeakers con
    !apos <- time' "Get ''s" $ getApostrophes con
    !bffs <- time' "Get relationships" $ getBffs con
    !aloof <- time' "Get aloof" $ getAloof con
    !amaze <- time' "Get amaze" $ getAmazed con
    !excite <- time' "Get excite" $ getExcited con
    !yell <- time' "Get yell" $ getYell con
    let printify = (mapSnd print' <$>)
    let col1 = toColumn (printify users) "Messages" 10
    let col2 = toColumn (printify bars) "Active" 10
    let col3 = toColumn (printify avgwl) "AWL" 6
    let col4 = toColumn (printify avgwc) "AWC" 6

    let col5 = toColumn randTop "Random Message" 68

    let us = fst <$> users
    let rows = formatTable us "User" 10 [col1, col2, col3, col4, col5]
    let rendered = unlines $ [ makeTimeScript "Activity (UTC)" activity
                             , withHeading "Top Users" $ rows
                             , headerTable "Broken Keyboard" ("Name", "YELLING") yell
                             , headerTable "Overexcited" ("Name", "!!!!!!!!!!!!!!") excite
                             , headerTable "Amazed" ("Name", "Times dumbfounded") amaze
                             , headerTable "Aloof" ("Name", "Has No Interest In This Number of Individuals") aloof
                             , headerTable "Apostrophe Users" ("Name", "Percent of Messages with ''s") apos
                             , headerTable "Can't English" ("Name", "Text Speak Count") text
                             , headerTable "Naysayers" ("Name", "Percent Negative") nay
                             , headerTable "Repeated Phrases" ("Phrase", "Times Repeated") repSimple
                             , headerTable "Longer Repeated Phrases" ("Phrase", "Times Repeated") repComplex
                             , headerTable "Clueless" ("Name", "Number Of Questions Asked") questions
                             , headerTable "Relationships" ("Mention", "Times") bffs
                             , headerTable "Clingy" ("Name", "Times Mentioning Someone") needy
                             , headerTable "Popular" ("Name", "Times Mentioned") mentions
                             , headerTable "Lonely Chatters" ("Name", "Times Talking to Self") self
                             , headerTable "Unique Nicks" ("Name","Messages") unique
                             , headerTable "Some Random URLs" ("Name", "URL") urls
                             , headerTable "Random Messages" ("Name", "Message") rand
                             , headerTable "Most Changed Nicks" ("Name", "Times Changed") nicks
                             , headerTable "Prolific Kickers" ("Name", "Times kicking") kickers
                             , headerTable "Trouble Makers" ("Name", "Times Kicked") kickees
                             , headerTable "Topics" ("Name", "Topic") topics
                             ]

    writeFile "generated.html" $ makeFile (linkLinks rendered) "css.css" ["util.js"]

doAction :: Action -> IO ()
doAction action = do
    con <- connect
    case action of
        Repopulate -> repopulateDb con
        Generate -> generate con
    disconnect con
