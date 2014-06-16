{-# LANGUAGE DoAndIfThenElse, NoMonomorphismRestriction, BangPatterns #-}
module StarStats.DB.Driver where

import Prelude hiding (foldl, concat, sequence_, sum)
import Control.Applicative
import Control.Exception
import Data.List (concat, isInfixOf)
import Data.Maybe
import Database.HDBC
import Database.HDBC.ODBC
import qualified Data.Text as T
import qualified StarStats.Parsers.Irssi as Irssi
import StarStats.Renderer
import StarStats.Utils
import StarStats.DB.Utils
import StarStats.DB.Tables
import StarStats.DB.Queries
import StarStats.DB.Connection
import StarStats.Watcher
import StarStats.Log.Log

getChanName :: String -> String
getChanName n =
    let s = T.pack n in
    let usc '_' = True
        usc _ = False in
    let l = T.split usc s in
    concat $ (case T.unpack <$> l of
        (x:xs) -> xs
        l -> l)

generate :: IConnection c => ServerInfo -> c -> IO ()
generate (ServerInfo driver dbName) con = do
    let chanName = getChanName dbName
    logInfo "Running queries"
    let timeGet s f = time' s $ f con
    _           <- timeGet "P Top"              populateTop
    _           <- timeGet "P Unique"           populateUnique
    _           <- timeGet "P Commit"           commit
    !users      <- timeGet "Q Message Count"    getMessageCount
    !tups       <- timeGet "Q User activity"    getTimes
    !randTop    <- timeGet "Q Random top"       getRandTop
    !lastseentop<- timeGet "Q Last Seen Top"    getLastSeenTop
    !rand       <- timeGet "Q Random"           getRandMessages
    !rant       <- timeGet "Q Rant"             getRandomRant
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
    !avgwc      <- timeGet "Q WPL"              getAverageWordsPerLine
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

    let printify = (mapSnd print' <$>)
    let bars = (toTimeBars tups)
    let ucol1 = toColumn (printify users) "Messages" "11%"
    let ucol2 = toColumn (printify bars) "Active" "107"
    let ucol3 = toColumn (printify avgwl) "AWL" "6%"
    let ucol4 = toColumn (printify avgwc) "WPL" "6%"
    let ucol5 = toColumn randTop "Random Message" "59%"
    let ucol6 = toColumn lastseentop "Last Seen" "160"

    let uus = fst <$> users
    let urows = formatTable "Top Users"
                            "User ranking by number of lines spoken (all time). This also includes the average words length (AWL) and average words per line (WPL) as well as a breakdown of activity per quarter of the day, a random message, and when the user last spoke."
                             uus "User" "18%" [ucol1, ucol2, ucol3, ucol4, ucol5, ucol6]

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
                                  "Lines ranked by the number of times they have been repeated, ignoring case and punctuation."
                                  ("Message", "44%")
                                  ("Times", "10%")
                                  ("Last Said By", "11%")
                                  ("Last Said On", "25%")

    let testScript = "<script>\
                    \ triggerPointer();\
                    \ </script>"
    let rcrows = table4 repComplex "Complex Repeated Phrases"
                                   "Longer lines ranked by the number of times they have been repeated, ignoring case and punctuation."
                                   ("Message", "44%")
                                   ("Times", "10%")
                                   ("Last Said By", "11%")
                                   ("Last Said On", "25%")
    let urlrows = table4 topUrls "Top URLs"
                                 "Urls ranked by the number of times they have been posted."
                                 ("URL", "44%")
                                 ("Times", "10%")
                                 ("Last Said By", "11%")
                                 ("Last Said On", "25%")
    let donutGraphs = [ makeDonut "Hourly Activity"
                                  "Relative activity in the channel broken down by hour (UTC)."
                                  "hourly-canvas"
                                  hourly
                      , makeHalfDonut "Daily Activity"
                                      "Relative activity in the channel broken down by day of the week."
                                       "daily-canvas"
                                       daily
                      ]
    let lineGraphs = [ makeLine "Monthly Activity"
                                "Activity in the channel broken down by the most recent 12 months."
                                "monthly-canvas"
                                "messages"
                                monthly
                     , makeLine "Active Users"
                                "The number of active users broken down by the most recent 12 months."
                                "users-canvas"
                                "users"
                                activet
                 ]
    let headerTable50 = headerTable "50%" "50%" False
    let headerTable2080 = headerTable "20%" "80%" False
    let shortTable = headerTable "200" "200" False
    let tables = [ urows
                 , headerTable2080 "A random selection of channel topics."
                                   "Random Topics"
                                   "Name"
                                   "Topic"
                                   topics
                 , headerTable2080 "A random selection of posted URLs."
                                   "Some Random URLs"
                                   "Name"
                                   "URL"
                                   urls
                 , headerTable2080 "A random selection of spoken lines."
                                   "Random Messages"
                                   "Name"
                                   "Message"
                                   rand
                 , headerTable "20%"
                               "80%"
                               True
                               "A random long message from some user."
                               "Random Rant"
                               "Name"
                               "Message"
                               rant
                 , rsrows
                 , rcrows
                 , urlrows
                 , headerTable50 "The percent of lines matching friendly greetings such as 'welcome'."
                                 "Friendly"
                                 "Name"
                                 "Times"
                                 friendly
                 , headerTable50 "The number of hours spent in the channel divided by lines spoken."
                                 "Champion Idlers"
                                 "Name"
                                 "Idle Quotient"
                                 idlers
                 , headerTable50 "The percentage of messages in ALL CAPS."
                                 "Enthusiastic"
                                 "Name"
                                 "YELLING (%)"
                                 yell
                 , headerTable50 "The percentage of lines this user has written that are very long."
                                 "Loquatious"
                                 "Name"
                                 "Verbose (%)"
                                 loq
                 , headerTable50 "The percent of lines containing exclamation points."
                                 "Excitable"
                                 "Name"
                                 "!!!!!!!!!!!!!! (%)"
                                 excite
                 , headerTable50 "The percent of lines matching shocked/surprised words such as 'wow'"
                                 "Amazed"
                                 "Name"
                                 "Lost for Words (%)"
                                 amaze
                 , headerTable50 "The percent of line's containing apostrophe's."
                                 "Apostrophe Users"
                                 "Name"
                                 "Message's (%)"
                                 apos
                 , headerTable50 "The percent of lines containing text speak."
                                 "Redefining English"
                                 "Name"
                                 "Text Speak (%)"
                                 text
                 , headerTable50 "Average words per line + average characters per word."
                                 "Well Spoken"
                                 "Name"
                                 "Eloquence Quotient"
                                 wellspoken
                 , headerTable50 "The percent of lines containing the word 'no'."
                                 "Naysayers"
                                 "Name"
                                 "Negativity (%)"
                                 nay
                 , headerTable50 "The percent of messages containing question marks."
                                 "Inquisitive"
                                 "Name"
                                 "Questions Asked (%)"
                                 questions
                 , headerTable50 "The number of times this user has mentioned another (recently active) user."
                                 "Sociable"
                                 "Name"
                                 "Times Mentioning Someone"
                                 needy
                 , headerTable50 "The number of times this user has been mentioned by another (recently active) user."
                                 "Popular"
                                 "Name"
                                 "Times Mentioned"
                                 popular
                 , headerTable50 "The number of times this user has spoken many consecutive lines in a row."
                                 "A Lot to Say"
                                 "Name"
                                 "Times Talking to Self"
                                 self
                 , headerTable50 "Users who have spoken in the past few days."
                                 "Recently Active Users"
                                 "Name"
                                 "Messages"
                                 unique
                 , headerTable50 "A count of the number of times this user has changed their nick."
                                 "Most Changed Nicks"
                                 "Name"
                                 "Times Changed"
                                 nicks
                 , headerTable50 "Those who have kicked many users."
                                 "Prolific Kickers"
                                 "Name"
                                 "Times Kicking"
                                 kickers
                 , headerTable50 "Those who have been kicked many times."
                                 "Trouble Makers"
                                 "Name"
                                 "Times Kicked"
                                 kickees
                 ]
    let donutGraphSection = section $ catMaybes donutGraphs
    let lineGraphSection = section $ catMaybes lineGraphs
    let tableSection = section $ catMaybes tables
    let heading = divId "lead" $ tag "h1" ("#" ++ chanName)
    timeInfo <- getTimeInfo con
    let error' x = (divClass "tribox") (divId "emptyhead" x ++ divClass "tritext empty" "")
    let bottom = case timeInfo of
                   Just t -> t
                   Nothing -> error' $ ("no data added yet!")
    let content = divId "content" $ linkLinks (donutGraphSection ++ lineGraphSection ++ tableSection ++ bottom ++ testScript)
    putStrLn $ makeFile (heading ++ content) "/css.css" (getTitle chanName) ["/util.js", "http://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js", "highcharts.js", "exporting.js"]
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

withConnection :: ServerInfo -> (ConnWrapper -> IO ()) -> IO ()
withConnection sinfo f = do
    con <- connect sinfo
    let wrapped = ConnWrapper con
    f wrapped
    close wrapped

doAction :: Bool -> Action -> ServerInfo -> IO ()
doAction reset action sinfo = do
    if reset
    then doAction False Initialize sinfo
    else return ()
    case action of
        Generate -> do
            logInfo "Generating webpage"
            withConnection sinfo (safeGenerate sinfo)
        Watch parser file -> do
            logInfo "Watch file"
            withConnection sinfo
                (watch file . insertLine parser)
        OnlyWatch parser file -> do
            logInfo "Watch file only"
            withConnection sinfo
                (watchFull False file . insertLine parser)
        Insert parser file -> do
            logInfo "Inserting file into the database"
            withConnection sinfo
                (populate file . insertLine parser)
        Initialize -> do
            logInfo "Initializing databases"
            withConnection sinfo initDb
