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
--    while True:
--        newSize = getSize(filename)
--        if newSize > size:
--            print(getEnd(filename, newSize - size)[:-1])
--            sys.stdout.flush()
--            ct += 1
--            checkCt(ct)
--            size = newSize

--def main():
--    ct = 0
--    t = 0
--    filename = sys.argv[1]
--    if not os.path.isfile(filename):
--        sys.stderr.write('No such file "%s"' % filename)
--        sys.exit(1)
--    size = getSize(filename)
--    repopulate = "-r" in sys.argv
--    killeof = "-k" in sys.argv
--    if repopulate:
--        lines = [line for line in open(filename)]
--        for line in lines:
--            print(line, end="")
--            sys.stdout.flush()
--            ct += 1
--            checkCt(ct)
--        if killeof:
--            print("")
--            sys.exit(1)

--        time.sleep(1)
--threadDelay
