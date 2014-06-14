{-# LANGUAGE DoAndIfThenElse, BangPatterns #-}
module StarStats.Watcher where

import Prelude hiding(putStrLn, putStr, readFile)
import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import qualified Data.ByteString as C
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
import qualified StarStats.File as File
import StarStats.DB.Tables
import StarStats.DB.Utils
import StarStats.DB.Connection
import StarStats.Log.Log



watch :: FilePath --to populate from
      -> (String -> IO ())
      -> IO ()
watch fp f = do
    let file = File.mkFile fp
    size <- File.getSize file
    populate fp f
    helper file size
    where helper file size = do
            threadDelay 1000000
            newSize <- File.getSize file
            if (newSize > size)
            then do (ls, newFile) <- File.readEnd file (newSize - size)
                    sequence_ $ f <$> ls
                    helper newFile newSize
            else helper file newSize


populate :: FilePath
         -> (String -> IO ())
         -> IO ()
populate fp action = do
    let file = File.mkFile fp
    checkFile file
    File.processLines file action


checkFile :: File.File -> IO ()
checkFile file = do
    size <- File.getSize file
    if toInteger size > 50000000
    then do logWarning "File size greater than 50MB detected."
            logWarning "This may cause issues due to a memory leak in native libraries."
            logWarning "If so, split the file into several pieces and then insert each piece."
            error ""
    else return ()
{-getSize :: String -> IO FileOffset
getSize file = do
    fileSize <$> getFileStatus file

getEnd :: String -> FileOffset -> IO String
getEnd file bytes = do
    handle <- openBinaryFile file ReadMode
    hSeek handle SeekFromEnd (- (toInteger bytes))
    return ""
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
    sequence_ $ safePutLn <$> ls

watchFile :: String -> FileOffset -> IO ()
watchFile file size = do
    newSize <- getSize file
    if newSize > size
    then do end <- getEnd file (newSize - size)
            safePutStr end
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
    where matching (_, (Date t)) = localDay t == localDay t0
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

safePutLn :: String -> IO ()
safePutLn s = do
    x <- try (putStrLn s) :: IO (Either SomeException ())
    hFlush stdout
    case x of
        Left _ -> return $ error "BAD STRING"
        Right _ -> return ()

safePutStr :: String -> IO ()
safePutStr s = do
    x <- try (putStr s) :: IO (Either SomeException ())
    hFlush stdout
    case x of
        Left _ -> return $ error "BAD STRING"
        Right _ -> return ()

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
                            sequence_ $ safePutLn <$> ss
                            logInfo "Resuming watch"
    watchFile file size-}

