{-# LANGUAGE DoAndIfThenElse #-}
module IRCDB.Watcher where

import Control.Applicative
import System.Posix.Files
import System.Posix.Types
import System.IO
import Data.ByteString.Char8 hiding (putStr, putStrLn, readFile, lines)
import Data.ByteString.UTF8 hiding (lines)
import Control.Concurrent

import System.Directory

getSize :: String -> IO FileOffset
getSize file = do
    fileSize <$> getFileStatus file

getEnd :: String -> FileOffset -> IO String
getEnd file bytes = do
    handle <- openBinaryFile file ReadMode
    hSeek handle SeekFromEnd (- (toInteger bytes))
    s <- hGet handle (fromIntegral bytes)
    hClose handle
    return $ toString s


watch :: String -> Bool -> IO ()
watch file doRepop = do
    exists <- doesFileExist file
    if not exists
    then error "no file"
    else do if doRepop
            then repopulate file
            else return ()
            size <- getSize file
            watchFile file size


repopulate :: String -> IO ()
repopulate file = do
    ls <- lines <$> readFile file
    sequence_ $ putStrLn <$> ls

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
