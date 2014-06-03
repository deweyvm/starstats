{-# LANGUAGE DoAndIfThenElse, NoMonomorphismRestriction, BangPatterns #-}
module IRCDB.DB.Driver where

import Prelude hiding (foldl, concat, sequence_, sum)
import Control.Applicative
import Data.List (concat)
import Database.HDBC
import Database.HDBC.ODBC
import GHC.IO.Encoding
import IRCDB.Renderer
import IRCDB.DB.Utils
import IRCDB.DB.Tables
import IRCDB.DB.Queries

data Action = Repopulate | Generate

connect :: String -> String ->IO Connection
connect driver dbName = do
    let connectionString = "DSN=name32;\
                          \ Driver={" ++ driver ++ "};\
                          \ Server=localhost;\
                          \ Port=3306;\
                          \ Database=" ++ dbName ++ ";\
                          \ User=root;\
                          \ Password=password;\
                          \ Option=3;"
    conn <- connectODBC connectionString
    return conn


generate :: IConnection c => String -> c -> IO ()
generate dbName con = do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8
    let timeGet s f = time' s $ f con
    _           <- timeGet "P Delete Temp"      deleteTemps
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
    !rltships   <- timeGet "Q Relationships"    getRelationships
    !amaze      <- timeGet "Q Amaze"            getAmazed
    !excite     <- timeGet "Q Excite"           getExcited
    !yell       <- timeGet "Q Yell"             getYell
    !wellspoken <- timeGet "Q Wellspoken"       getWellSpoken
    !welcoming  <- timeGet "Q Welcoming"        getWelcomers
    !idlers     <- timeGet "Q Idlers"           getIdlers
    let printify = (mapSnd print' <$>)
    let bars = (toTimeBars tups)
    let col1 = toColumn (printify users) "Messages" 10
    let col2 = toColumn (printify bars) "Active" 10
    let col3 = toColumn (printify avgwl) "AWL" 6
    let col4 = toColumn (printify avgwc) "AWC" 6

    let col5 = toColumn randTop "Random Message" 68

    let us = fst <$> users
    let rows = formatTable us "User" 10 [col1, col2, col3, col4, col5]
    let tables = [ makeTimeScript "Activity (UTC)" activity
                 , withHeading "Top Users" $ rows
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
                 , headerTable "Relationships"
                               "Mention"
                               "Times"
                               rltships
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

    putStrLn $ makeFile (heading ++ (linkLinks contents)) "/css.css" (getTitle dbName) ["/util.js"]

getTitle :: String -> String
getTitle s = tag "title" ("Stats for #" ++ s)

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
    return $ unlines [ tag "h1" ("Stats for #" ++ channel)
                     , genTag "div" [("id", "lead")] $ tag "p" desc
                     ]


doAction :: String -> String -> Action -> IO ()
doAction driver dbName action = do
    con <- connect driver dbName
    case action of
        Repopulate -> repopulateDb con
        Generate -> generate dbName con
    disconnect con
