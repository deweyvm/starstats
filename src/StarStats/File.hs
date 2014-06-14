{-# LANGUAGE DoAndIfThenElse, BangPatterns #-}
module StarStats.File(
    mkFile
  , readEnd
  , getSize
  , exists
  , processLines
  , File
) where

import Prelude hiding (readFile)
import Control.Applicative((<$>))
import Control.Exception
import qualified System.Directory as Dir
import System.IO.UTF8 hiding (readFile)
import qualified System.IO as IO
import System.IO.Error hiding (try)
import System.Posix.Files
import System.Posix.Types
import qualified Data.ByteString as C
import qualified Data.ByteString.UTF8 as U

import StarStats.Log.Log

data File = File FilePath (Maybe String)

exists :: File -> IO Bool
exists f = Dir.doesFileExist $ getPath f

getPath :: File -> FilePath
getPath (File p _) = p

getSize :: File -> IO FileOffset
getSize f  = do
    fileSize <$> getFileStatus (getPath f)

mkFile :: FilePath -> File
mkFile fp = File fp Nothing

processLines :: File -> (String -> IO ()) -> IO ()
processLines file f = do
    handle <- IO.openBinaryFile (getPath file) IO.ReadMode
    helper handle
    where helper handle = do
              line <- try $ hGetLine handle :: IO (Either IOError String)
              case line of
                  Left l | isEOFError l -> return ()
                  Left l | otherwise -> error $ show l
                  Right r -> do f r
                                helper handle


readEnd :: File -> FileOffset -> IO ([String], File)
readEnd f@(File p fmbs) bytes = do
    handle <- openBinaryFile p IO.ReadMode
    IO.hSeek handle IO.SeekFromEnd (- (toInteger bytes))
    s <- C.hGet handle (fromIntegral bytes)
    IO.hClose handle
    let us = U.toString s
    let (xs, mbs) = split us
    let (xs', fp') = mergePrev xs f
    return $ (xs', withBuffer fp' mbs)

mergePrev :: [String] -> File -> ([String], File)
mergePrev (l:ls) f@(File fp (Just x)) = ((x ++ l):ls, remBuffer f)
mergePrev (l:ls) fp = ((l:ls), fp)
mergePrev [] fp = undefined --impossible

merge :: [String] --full lines
      -> Maybe String -- partial string read
      -> Maybe String -- partial string previous
      -> File
      -> ([String], File)
merge (l:ls) mb (Just x) f = ((x ++ l):ls, withBuffer f mb)
merge (l:ls) mb Nothing f = (l:ls, withBuffer f mb)
merge [] mbr mbp f = ([], f)


remBuffer :: File -> File
remBuffer (File p _) = File p Nothing

withBuffer :: File -> Maybe String -> File
withBuffer (File p _) buf = File p buf

split :: String -> ([String], Maybe String)
split s =
    let splitted = splitOn (== '\n') s in
    case reverse s of
        ('\n':xs) -> (splitted, Nothing)
        xs -> splitLast splitted


splitLast :: [a] -> ([a], Maybe a)
splitLast xs =
    case reverse xs of
        (x:xs) -> (reverse xs, Just x)
        [] -> ([], Nothing)
splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =
    case dropWhile p s of
        "" -> []
        s' -> w : splitOn p s''
              where (w, s'') = break p s
