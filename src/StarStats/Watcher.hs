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



watch :: FilePath
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
                    sequence_ $ (\s -> if length s == 0 then return () else f s) <$> ls
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
