{-# LANGUAGE DoAndIfThenElse, NoMonomorphismRestriction, BangPatterns #-}
module StarStats.DB.Driver where

import Prelude hiding (foldl, concat, sequence_, sum)
import Control.Applicative
import Control.Exception
import Data.List (concat, isInfixOf)
import Data.Maybe
import Database.HDBC
import Database.HDBC.ODBC
import qualified StarStats.Parsers.Irssi as Irssi
import StarStats.Renderer
import StarStats.DB.Utils
import StarStats.DB.Tables
import StarStats.DB.Queries
import StarStats.DB.Connection
import StarStats.Watcher
import StarStats.Log.Log



generate :: IConnection c => ServerInfo -> c -> IO ()
generate (ServerInfo driver dbName) con = do
    logInfo "Running queries"
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
    !hourly     <- timeGet "Q Hourly activity"  getHourlyActivity
    !daily      <- timeGet "Q Daily activity"   getDailyActivity
    !topUrls    <- timeGet "Q Top Urls"         getTopUrls
    !( monthly
     , activet) <- timeGet "Q Monthly activity" getMonthlyActivity
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
    !amaze      <- timeGet "Q Amaze"            getAmazed
    !excite     <- timeGet "Q Excite"           getExcited
    !yell       <- timeGet "Q Yell"             getYell
    !wellspoken <- timeGet "Q Wellspoken"       getWellSpoken
    !loq        <- timeGet "Q Loquatious"       getLong
    !friendly  <- timeGet "Q Friendly"         getFriendly
    !idlers     <- timeGet "Q Idlers"           getIdlers
    logInfo "Assembling html"
    let di = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam cursus tellus eget justo sodales eleifend. Integer vel suscipit orci. Praesent accumsan est eget sem sagittis tristique. Vestibulum tincidunt lorem orci. Nunc quis eleifend quam. Ut vestibulum neque velit, a gravida purus consectetur nec. Ut non commodo nisl."
    let printify = (mapSnd print' <$>)
    let bars = (toTimeBars tups)
    let ucol1 = toColumn (printify users) "Messages" "11%"
    let ucol2 = toColumn (printify bars) "Active" "107"
    let ucol3 = toColumn (printify avgwl) "AWL" "6%"
    let ucol4 = toColumn (printify avgwc) "AWC" "6%"
    let ucol5 = toColumn randTop "Random Message" "59%"

    let uus = fst <$> users
    let urows = formatTable "Top Users" di uus "User" "18%" [ucol1, ucol2, ucol3, ucol4, ucol5]

    let table4 :: (Print a, Print b, Print c)
               => [(String, a, b, c)]
               -> String
               -> String
               -> (String, String)
               -> (String, String)
               -> (String, String)
               -> (String, String)
               -> Maybe String
        table4 xs h desc (h0, w0) (h1, w1) (h2, w2) (h3, w3)=
            let (col1, col2, col3) = split3 xs in
            let col1' = toColumn (printify col1) h1 w1 in
            let col2' = toColumn (printify col2) h2 w2 in
            let col3' = toColumn (printify col3) h3 w3 in
            let us = fst <$> col1 in
            formatTable h desc us h0 w0 [col1', col2', col3']
    let rsrows = table4 repSimple "Simple Repeated Phrases"
                                  di ("Message", "50%")
                                     ("Times", "10%")
                                     ("Last Said By", "11%")
                                     ("Last Said On", "19%")


    let rcrows = table4 repComplex "Complex Repeated Phrases"
                                   di ("Message", "50%")
                                      ("Times", "10%")
                                      ("Last Said By", "11%")
                                      ("Last Said On", "19%")
    let urlrows = table4 topUrls "Top URLs"
                                 di ("URL", "50%")
                                    ("Times", "10%")
                                    ("Last Said By", "11%")
                                    ("Last Said On", "19%")

    let graphs = [ makeTimeScript "Hourly Activity (UTC)" di "hourly"  hourly
                 , makeTimeScript "Daily Activity"        di "daily"   daily
                 , makeTimeScript "Monthly Activity"      di "monthly" monthly
                 , makeTimeScript "Active Users"          di "users"   activet
                 ]
    let tables = [ urows
                 , headerTable di
                               "Random Topics"
                               "Name"
                               "Topic"
                               topics
                 , urlrows
                 , headerTable di
                               "Friendly"
                               "Name"
                               "Times"
                               friendly
                 , headerTable "The number of hours spent in the channel divided by lines spoken"
                               "Champion Idlers"
                               "Name"
                               "Idle Quotient"
                               idlers
                 , headerTable "The percentage of messages in ALL CAPS."
                               "Enthusiastic"
                               "Name"
                               "YELLING (%)"
                               yell
                 , headerTable "The percentage of lines this user has written that are very long"
                               "Loquatious"
                               "Name"
                               "Verbose (%)"
                               loq
                 , headerTable "The percent of lines containing exclamation points."
                               "Excitable"
                               "Name"
                               "!!!!!!!!!!!!!! (%)"
                               excite
                 , headerTable di
                               "Amazed"
                               "Name"
                               "Lost for Words (%)"
                               amaze
                 , headerTable "The percent of line's containing apostrophe's."
                               "Apostrophe Users"
                               "Name"
                               "Message's (%)"
                               apos
                 , headerTable "The percent of lines containing text speak."
                               "Redefining English"
                               "Name"
                               "Text Speak (%)"
                               text
                 , headerTable "Average words per line + average characters per word."
                               "Well Spoken"
                               "Name"
                               "Eloquence Quotient"
                               wellspoken
                 , headerTable "The percent of lines containing the word 'no'"
                               "Naysayers"
                               "Name"
                               "Negativity (%)"
                               nay
                 , rsrows
                 , rcrows
                 , headerTable "The percent of messages containing question marks."
                               "Inquisitive"
                               "Name"
                               "Questions Asked (%)"
                               questions
                 , headerTable "The number of times this user has mentioned another (recently active) user."
                               "Sociable"
                               "Name"
                               "Times Mentioning Someone"
                               needy
                 , headerTable "The number of times this user has been mentioned by another (recently active) user."
                               "Popular"
                               "Name"
                               "Times Mentioned"
                               popular
                 , headerTable "The number of times this user has spoken many consecutive lines in a row."
                               "A Lot to Say"
                               "Name"
                               "Times Talking to Self"
                               self
                 , headerTable "Users who have spoken in the past few days."
                               "Recently Active Users"
                               "Name"
                               "Messages"
                               unique
                 , headerTable di
                               "Some Random URLs"
                               "Name"
                               "URL"
                               urls
                 , headerTable di
                               "Random Messages"
                               "Name"
                               "Message"
                               rand
                 , headerTable di
                               "Most Changed Nicks"
                               "Name"
                               "Times Changed"
                               nicks
                 , headerTable "Those who have kicked many users."
                               "Prolific Kickers"
                               "Name"
                               "Times Kicking"
                               kickers
                 , headerTable "Those who have been kicked many times."
                               "Trouble Makers"
                               "Name"
                               "Times Kicked"
                               kickees
                 ]
    let graphSection = section $ catMaybes graphs
    let tableSection = section $ catMaybes tables
    let heading = divId "lead" $ tag "h1" ("#" ++ dbName)
    timeInfo <- getTimeInfo con
    let error' x =( divClass "tribox" ) (divId "emptyhead" x ++ divClass "tritext empty" "")
    let bottom = case timeInfo of
                   Just t -> t
                   Nothing -> error' $ ("no data added yet!")
    let content = divId "content" $ linkLinks (graphSection ++ tableSection ++ bottom)
    putStrLn $ makeFile (heading ++ content) "/css.css" (getTitle dbName) ["/util.js"]
    logInfo "Finished"

getTitle :: String -> String
getTitle s = tag "title" ("Stats for #" ++ s ++ " -- starstats")


getTimeInfo :: IConnection c => c -> IO (Maybe String)
getTimeInfo con = do
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
    return $ if words' == 0
             then Nothing
             else Just $ divClass "summary" desc

safeGenerate :: IConnection c => ServerInfo -> c -> IO ()
safeGenerate sinfo con = do
    e <- try (generate sinfo con) :: IO (Either IOError ())
    case e of
        Left l -> if isInfixOf "Deadlock" (show l)
                  then do logWarning "Deadlock encountered, retrying"
                          safeGenerate sinfo con
                  else logError (show l)
        Right _ -> return ()

doAction :: Action -> ServerInfo -> IO ()
doAction action sinfo = do
    con <- connect sinfo
    case action of
        Read parser -> do
            logInfo "Reading data lines from stdin"
            readDb parser con
        Generate -> do
            logInfo "Generating webpage"
            safeGenerate sinfo con
        Recover parser file -> do
            logInfo "Recovering from log"
            watch parser file False True sinfo
        Repopulate parser file -> do
            logInfo "Repopulating database"
            watch parser file True False sinfo
        Initialize -> do
            logInfo "Initializing databases"
            initDb con
    close con
