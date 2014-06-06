{-# LANGUAGE DoAndIfThenElse, FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
module StarStats.DB.Utils where

import Control.DeepSeq
import Control.Applicative
import Criterion.Measurement
import Data.Convertible
import Data.List
import Data.Maybe
import Database.HDBC
import Text.Printf
import System.Directory


data Action = Recover String {- Scan the given file for the last message
                                inserted. Print all subsequent data lines
                                and then resume watching. -}
            | Repopulate String {- Repopulate from the beginning of the given
                                   file and then resume watching. -}
            | Read {- | Read and insert data lines from stdin. -}
            | Generate {- | Generate html. -}
data ServerInfo = ServerInfo String String

linkLinks :: String -> String
linkLinks s = replaceUrls s (\x -> "<a href=\"" ++ x ++ "\">" ++ x ++ "</a>")

escapeHtml :: String -> String
escapeHtml =
    replaceChar '>' "&gt;" . replaceChar '<' "&lt;"

removeUrls :: String -> String
removeUrls s = replaceUrls s (\x -> "")


isUrl :: String -> Bool
isUrl ('h':'t':'t':'p':':':'/':'/':_) = True
isUrl ('h':'t':'t':'p':'s':':':'/':'/':_) = True
isUrl _ = False

replaceUrls :: String -> (String -> String) -> String
replaceUrls s f =
    let words' = words s in
    unwords $ (\x -> (if isUrl x then f else id) x) <$> words'

extractUrl :: String -> String
extractUrl s =
    let words' = words s in
    case filter isUrl words' of
        (x:_) -> x
        _ -> "!!Error extracting url!!"



replaceChar :: Char -> String -> String -> String
replaceChar c r s =
    concat $ helper c s r
    where helper :: Char -> String -> String -> [String]
          helper c (x:xs) r =
              if c == x
              then r : helper c xs r
              else [x] : helper c xs r
          helper c [] r = [[]]

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

halfList :: [a] -> ([a], [a])
halfList [] = ([], [])
halfList xs =
    helper xs [] 0
    where halflen = (length xs :: Int) `quot` 2
          helper (y:ys) first ct =
              if ct == halflen
                then ((y : first), ys)
                else helper ys (y : first) (ct + 1)

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



getTopBottom :: Int -> [a] -> ([a], [a])
getTopBottom _ [] = ([],[])
getTopBottom split xs
    | split `div` 2 > length xs = halfList xs
    | otherwise = let first = take split xs in
                  let last' = drop (length xs - split) xs in
                  (first, last')

extractSingle :: (Convertible SqlValue a, Default a)
              => [[SqlValue]]
              -> a
extractSingle ((x:_):_) = fromSql x
extractSingle         _ = default'

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

getSimple :: IConnection c
          => c
          -> ([[SqlValue]] -> a)
          -> String
          -> IO a
getSimple con f query = do
    res <- runQuery con query
    return $ f res

fromSqlString :: SqlValue -> String
fromSqlString v =
    let s = fromSql v :: String in
    escapeHtml s


time' :: NFData a => String -> IO a -> IO a
time' msg action = do
    (s, res) <- time $ force <$> action
    let len = length msg
    let whitespace = printf ("%" ++ show (27 - len) ++ "s") " " ++ "\t"
    putStrLn ("<!-- " ++ msg ++ ": " ++ whitespace ++ printf "%.3fs" s ++ " -->")
    return res
