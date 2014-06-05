{-# LANGUAGE DoAndIfThenElse, BangPatterns #-}
module IRCDB.Watcher where


import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as U
import Data.Time.LocalTime
import System.Posix.Files
import System.Posix.Types
import System.Directory
import System.IO
import Debug.Trace
import IRCDB.Parser
import IRCDB.Time
import IRCDB.DB.Tables
import IRCDB.DB.Connection

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


watch :: String -> Bool -> Bool -> String -> String -> IO ()
watch file doRepop doRecover driver chanName = do
    exists <- doesFileExist file
    if not exists
    then error "no file"
    else do (if doRepop
            then repopulate file
            else return ())
            (if doRecover && False
            then recover file driver chanName
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

parseGood :: [String] -> [(String, DataLine)]
parseGood (x:xs) =
    case parseLine x of
        Left l -> parseGood xs
        Right r -> (x, r) : parseGood xs
parseGood [] = []

-- Get latest message and then read backwards until we find it and parse from there
recover :: String -> String -> String -> IO ()
recover file driver chanName = do
    con <- connect driver chanName
    latest <- getLatestMessage con chanName -- \ these two lines
    size <- getSize file                    -- / should be atomic together

    case latest of
        Nothing -> error "No need to recover"
        Just (msg, t) -> do hPutStr stderr "Recovering\n"
                            ls <- lines <$> readFile file
                            let ps = parseGood ls
                            let matchingDate = matchDate t ps
                            let matchingLine = matchLine t msg matchingDate
                            let ss = fst <$> matchingLine
                            hPutStr stderr $ "Let's do it: " ++ show (length ss) ++ "\n"
                            !x <- sequence_ $ (\x -> putStrLn x *> hFlush stdout ) <$> ss
                            hPutStr stderr $ "Done\n"
    hPutStr stderr $ "Closing connection\n"
    close con
    watchFile file size

