{-# LANGUAGE DoAndIfThenElse, FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
module IRCDB.DB.Utils where

import Control.Applicative
import Data.Convertible
import Data.List
import Data.Maybe
import Database.HDBC
import qualified Text.Regex.Posix as REP
import qualified Text.Regex as RE
import Text.Printf
import System.Directory


replace :: String -> String -> String -> String
replace x y z = RE.subRegex (RE.mkRegex x) z y

linkLinks :: String -> String
linkLinks x = replace urlRegexp "<a href=\"\\0\">\\0</a>" x

escapeHtml :: String -> String
escapeHtml = replace ">" "&gt;" . replace "<" "&lt;"

class Print a where
    print' :: a -> String

instance Print Int where
    print' = show

instance Print Double where
    print' = printf "%.2f"

instance Print String where
    print' = id


class Default a where
    default' :: a

instance Default Double where
    default' = 0

instance Default Int where
    default' = 0

instance Default [Char] where
    default' = ""

configFile :: String
configFile = "config"

readConfig :: IO String
readConfig = do
    exists <- doesFileExist configFile
    if not exists
    then
        error $ "file \"" ++ configFile ++ "\" not found"
    else do
        config <- lines <$> readFile configFile
        return $ processConfig config
    where processConfig (c:_) = c
          processConfig     _ = error "file 'config' is empty"


insertHalfway :: [a] -> a -> [a]
insertHalfway [] x = [x]
insertHalfway xs x =
    helper xs 0
    where halflen = (length xs :: Int) `quot` 2
          helper (y:ys) ct =
              if ct == halflen
                then (x : y : ys)
                else (y : helper ys (ct + 1))

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x, y) = (x, f y)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x


assemble :: [(String, Int, Int)] -> [[(String, Int, Int)]]
assemble xs = groupBy (\(n, _, _) (m, _, _) -> n == m) xs


assemble2 :: [[(String, Int, Int)]] -> [(String, Int, Int, Int, Int)]
assemble2 xs =
    let grabAll vs@((name,_,_):_) =
            let getWhere i = fromMaybe 0 (thd3 <$> (find (\(nn, ii, _) -> i == ii && name == nn) vs)) in
            let w = getWhere 0
                x = getWhere 1
                y = getWhere 2
                z = getWhere 3 in
            (name, w, x, y, z) in
    grabAll <$> xs

urlRegexp :: String
urlRegexp = "http://[^ ]*"

extractUrl :: String -> String
extractUrl s = case s REP.=~ urlRegexp :: [[String]] of
    ((x:_) : _) -> x
    _ -> "Error extracting url"


extractTup :: (Convertible SqlValue a
              , Convertible SqlValue b
              , Default a
              , Default b)
           => [SqlValue]
           -> (a, b)
extractTup (x:y:_) = (fromSql x, fromSql y)
extractTup       _ = (default', default')

type Extract a = [SqlValue] -> a

runQuery :: IConnection c => c -> String -> IO [[SqlValue]]
runQuery con q = quickQuery con q []

getAndExtract :: IConnection c
              => c
              -> [String]
              -> Extract a
              -> String
              -> IO [a]
getAndExtract con qs f query = do
    sequence_ $ runQuery con <$> qs
    res <- runQuery con query
    return $ f <$> res


fromSqlString :: SqlValue -> String
fromSqlString v =
    let s = fromSql v :: String in
    escapeHtml s
