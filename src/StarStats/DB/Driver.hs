{-# LANGUAGE DoAndIfThenElse, NoMonomorphismRestriction, BangPatterns #-}
module StarStats.DB.Driver where

import Prelude hiding (foldl, concat, sequence_, sum)
import Control.Applicative
import Data.List (concat)
import Database.HDBC
import Database.HDBC.ODBC
import GHC.IO.Encoding hiding (close)
import StarStats.Renderer
import StarStats.DB.Utils
import StarStats.DB.Tables
import StarStats.DB.Queries
import StarStats.DB.Connection
import StarStats.Watcher



generate :: IConnection c => String -> c -> IO ()
generate dbName con = do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8
    let timeGet s f = time' s $ f con
    _           <- timeGet "P Top"              populateTop
    _           <- timeGet "P Unique"           populateUnique
    _           <- timeGet "P Commit"           commit
    !users      <- timeGet "Q Message Count"    getMessageCount
    !tups       <- timeGet "Q User activity"    getTimes
    !randTop    <- timeGet "Q Random top10"     getRandTopTen
    !rand       <- timeGet "Q Random"           getRandMessages
    !nicks      <- timeGet "Q Nick changes"     getNicks
    !kickers    <- timeGet "Q Kickers"          getKickers
    !kickees    <- timeGet "Q Kickees"          getKickees
    !topics     <- timeGet "Q Random topics"    getRandTopics
    !urls       <- timeGet "Q Random urls"      getUrls
    !activity   <- timeGet "Q Overall activity" getOverallActivity
    !unique     <- timeGet "Q Unique nicks"     getUniqueNicks
    !avgwc      <- timeGet "Q AWC"              getAverageWordCount
    !avgwl      <- timeGet "Q AWL"              getAverageWordLength
    !self       <- timeGet "Q Consecutive msgs" getSelfTalk
    !popular    <- timeGet "Q Popular"          getPopular
    !needy      <- timeGet "Q Needy"            getNeedy
    !questions  <- timeGet "Q Questions"        getQuestions
    !repSimple  <- timeGet "Q Simple repeated"  getRepeatedSimple
    !repComplex <- timeGet "Q Complex repeated" getRepeatedComplex
    !nay        <- timeGet "Q Naysayers"        getNaysayers
    !text       <- timeGet "Q txt spk"          getTextSpeakers
    !apos       <- timeGet "Q ''s"              getApostrophes
    -- !rltships   <- timeGet "Q Relationships"    getRelationships
    !amaze      <- timeGet "Q Amaze"            getAmazed
    !excite     <- timeGet "Q Excite"           getExcited
    !yell       <- timeGet "Q Yell"             getYell
    !wellspoken <- timeGet "Q Wellspoken"       getWellSpoken
    !welcoming  <- timeGet "Q Welcoming"        getWelcomers
    !idlers     <- timeGet "Q Idlers"           getIdlers
    let printify = (mapSnd print' <$>)
    let bars = (toTimeBars tups)
    let col1 = toColumn (printify users) "Messages" 11
    let col2 = toColumn (printify bars) "Active" 10
    let col3 = toColumn (printify avgwl) "AWL" 6
    let col4 = toColumn (printify avgwc) "AWC" 6

    let col5 = toColumn randTop "Random Message" 63

    let us = fst <$> users
    let rows = formatTable "Top Users" us "User" 14 [col1, col2, col3, col4, col5]
    let tables = [ makeTimeScript "Activity (UTC)" activity
                 , rows
                 , headerTable "Welcoming"
                               "Name"
                               "Times"
                               welcoming
                 , headerTable "Champion Idlers"
                               "Name"
                               "Idle Quotient"
                               idlers
                 , headerTable "Broken Keyboard"
                               "Name"
                               "YELLING"
                               yell
                 , headerTable "Excitable"
                               "Name"
                               "!!!!!!!!!!!!!!"
                               excite
                 , headerTable "Amazed"
                               "Name"
                               "Times Dumbfounded"
                               amaze
                 , headerTable "Apostrophe Users"
                               "Name"
                               "Percent of Messages with ''s"
                               apos
                 , headerTable "Redefining English"
                               "Name"
                               "Text Speak Count"
                               text
                 , headerTable "Well Spoken"
                               "Name"
                               "Eloquence Quotient"
                               wellspoken
                 , headerTable "Naysayers"
                               "Name"
                               "Percent Negative"
                               nay
                 , headerTable "Repeated Phrases"
                               "Phrase"
                               "Times Repeated"
                               repSimple
                 , headerTable "Longer Repeated Phrases"
                               "Phrase"
                               "Times Repeated"
                               repComplex
                 , headerTable "Inquisitive"
                               "Name"
                               "Number Of Questions Asked"
                               questions
                 --, headerTable "Relationships"
                 --              "Mention"
                 --              "Times"
                 --              rltships
                 , headerTable "Sociable"
                               "Name"
                               "Times Mentioning Someone"
                               needy
                 , headerTable "Popular"
                               "Name"
                               "Times Mentioned"
                               popular
                 , headerTable "A Lot to Say"
                               "Name"
                               "Times Talking to Self"
                               self
                 , headerTable "Recently Active Users"
                               "Name"
                               "Messages"
                               unique
                 , headerTable "Some Random URLs"
                               "Name"
                               "URL"
                               urls
                 , headerTable "Random Messages"
                               "Name"
                               "Message"
                               rand
                 , headerTable "Most Changed Nicks"
                               "Name"
                               "Times Changed"
                               nicks
                 , headerTable "Prolific Kickers"
                               "Name"
                               "Times Kicking"
                               kickers
                 , headerTable "Trouble Makers"
                               "Name"
                               "Times Kicked"
                               kickees
                 , headerTable "Topics"
                               "Name"
                               "Topic"
                               topics
                 ]

    let contents = unlines tables
    heading <- makeHeading dbName con
    let content = (genTag "div" [("id", "content")] $ linkLinks contents)
    putStrLn $ makeFile (heading ++ content ++"<div id=\"footer\">Generated by <a href=\"https://github.com/deweyvm/starstats\">starstats</a> on Fri Jun  6 14:52:26 2014 in 0.0000 seconds.</div>") "/css.css" (getTitle dbName) ["/util.js"]
    --putStrLn $

getTitle :: String -> String
getTitle s = tag "title" ("Stats for #" ++ s ++ " -- starstats")

makeHeading :: IConnection c => String -> c -> IO String
makeHeading channel con = do
    !words' <- time' "H Get Words" $ getTotalWords con
    !msgs <- time' "H Get Messages" $ getTotalMessages con
    !start <- time' "H Get Start" $ getStartDate con
    !end <- time' "H Get End" $ getEndDate con
    let desc :: String
        desc = concat [ "Analyzed "
                      , show words'
                      , " words and "
                      , show msgs
                      , " messages from "
                      , start
                      , " to "
                      , end
                      , "."
                      ]
    let timeInfo = if words' == 0
                   then genTag "div" [("id", "emptyhead-wrapper")] $ genTag "div" [("id", "emptyhead")] "no data added yet!"
                   else genTag "div" [("id", "lead")] $ tag "p" desc
    return $ unlines [ tag "h1" ("Stats for #" ++ channel)
                     , timeInfo
                     ]


doAction :: Action -> ServerInfo -> IO ()
doAction action sinfo@(ServerInfo driver chanName) = do
    con <- connect driver chanName
    case action of
        Read -> readDb con
        Generate -> generate chanName con
        Recover file -> watch file False True sinfo
        Repopulate file -> watch file True False sinfo
    close con
