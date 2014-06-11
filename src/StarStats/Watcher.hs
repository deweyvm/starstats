{-# LANGUAGE DoAndIfThenElse, BangPatterns #-}
module StarStats.Watcher where

import Prelude hiding(putStrLn, putStr, readFile)
import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as U
import Data.Time.LocalTime
import System.Posix.Files
import System.Posix.Types
import System.Directory
import System.IO.UTF8
import System.IO (stdout, hFlush, hSeek, hClose, SeekMode(..), IOMode(..))
import Debug.Trace
import StarStats.Parsers.Common
import StarStats.Time
import StarStats.DB.Tables
import StarStats.DB.Utils
import StarStats.DB.Connection
import StarStats.Log.Log

getSize :: String -> IO FileOffset
getSize file = do
    fileSize <$> getFileStatus file

getEnd :: String -> FileOffset -> IO String
getEnd file bytes = do
    handle <- openBinaryFile file ReadMode
    hSeek handle SeekFromEnd (- (toInteger bytes))
    s <- C.hGet handle (fromIntegral bytes)
    hClose handle
    return $ U.toString s


watch :: DLParser -> String -> Bool -> Bool -> ServerInfo -> IO ()
watch parser file doRepop doRecover srv = do
    exists <- doesFileExist file
    if not exists
    then error "no file"
    else do (if doRepop
            then repopulate file
            else return ())
            (if doRecover
            then recover parser file srv
            else do size <- getSize file
                    watchFile file size)


repopulate :: String -> IO ()
repopulate file = do
    ls <- lines <$> readFile file
    sequence_ $ (\x -> putStrLn x *> hFlush stdout) <$> ls

watchFile :: String -> FileOffset -> IO ()
watchFile file size = do
    newSize <- getSize file
    if newSize > size
    then do end <- getEnd file (newSize - size)
            putStr end
            hFlush stdout
    else return ()
    threadDelay 1000000 --microseconds
    watchFile file newSize

getRunning :: (a -> Bool) -> [a] -> [a]
getRunning _ [] = []
getRunning f (x : xs) =
    if f x
    then xs
    else getRunning f xs

matchDate :: LocalTime -> [(String, DataLine)] -> [(String, DataLine)]
matchDate t0 ls  =
    getRunning matching ls
    where matching (_, (Day t)) = localDay t == localDay t0
          matching (_, (Open t)) = localDay t == localDay t0
          matching _ = False


matchLine :: LocalTime -> String -> [(String, DataLine)] -> [(String, DataLine)]
matchLine t0 msg0 ls =
    getRunning matching ls
    where matching (s, (Message t _ _ _)) =
              let tt0 = deleteSeconds t0 in
              let tt = deleteSeconds (setHoursMinutes t0 t) in
              tt == tt0  && s == msg0
          matching _ = False

parseGood :: DLParser -> [String] -> [(String, DataLine)]
parseGood parser (x:xs) =
    case parser x of
        Left l -> parseGood parser xs
        Right r -> zip (repeat x) r ++ parseGood parser xs
parseGood _ [] = []

-- Get latest message and then read backwards until we find it and parse from there
recover :: DLParser -> String -> ServerInfo -> IO ()
recover parser file srv@(ServerInfo _ dbName) = do
    logInfo "Making connection"
    con <- connect srv
    logInfo "Searching for latest message"
    latest <- getLatestMessage con dbName -- \ these two lines
    close con                             --
    size <- getSize file                  -- / should be atomic together

    case latest of
        Nothing -> do logWarning "No need to recover"
                      logWarning "Resuming watch"
        Just (msg, t) -> do ls <- lines <$> readFile file
                            let ps = parseGood parser ls
                            let matchingDate = matchDate t ps
                            let matchingLine = matchLine t msg matchingDate
                            let ss = fst <$> matchingLine
                            if length ss == 0
                            then do logWarning "Could not match log"
                                    logWarning "To recover, run a full repop"
                            else logInfo "Found match"
                            sequence_ $ (\x -> putStrLn x *> hFlush stdout ) <$> ss
                            logInfo "Resuming watch"
    watchFile file size

