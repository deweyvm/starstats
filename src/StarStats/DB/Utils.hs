{-# LANGUAGE DoAndIfThenElse, FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
module StarStats.DB.Utils where

import Control.DeepSeq
import Control.Applicative
import Criterion.Measurement
import Data.Convertible
import Data.List
import Data.Maybe
import Data.Char (toLower)
import qualified Data.Text as T
import Database.HDBC
import System.Directory
import Text.Printf
import StarStats.Log.Log
import StarStats.Parsers.Common
import StarStats.Utils
data ActionType = OnlyWatchT {- |  Watch the file for newly inserted messages-}
                | WatchT {- | Insert the contents of a file then watch it
                            for newly inserted messages. -}
                | InsertT {- | Repopulate from the beginning of the given
                                 file and then resume watching. -}
                | GenerateT {- | Generate html. -}
                | InitializeT {- | Initialize a blank database -}

data Action = OnlyWatch DLParser String
            | Watch DLParser String
            | Insert DLParser String
            | Generate
            | Initialize
data ServerInfo = ServerInfo String {- OBDC driver to use -}
                             String {- Db name to use. Can be any valid
                                       MySQL db name. Used to access the page
                                       through the cgi script-}


splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s =
    case dropWhile p s of
        "" -> []
        s' -> w : splitOn p s''
              where (w, s'') = break p s'

stripPunctuation :: String -> String
stripPunctuation = filter (\x -> not (elem x "\"';:.,?!"))

escapeHtml :: String -> String
escapeHtml =
    replaceChar '>' "&gt;" . replaceChar '<' "&lt;"

hyphenate :: String -> String
hyphenate =
    replaceChar ' ' "-" . replaceChar '\'' ""

removeUrls :: String -> String
removeUrls s = replaceUrls s (\x -> "")


isUrl :: String -> Bool
isUrl ('h':'t':'t':'p':':':'/':'/':_) = True
isUrl ('h':'t':'t':'p':'s':':':'/':'/':_) = True
isUrl _ = False


split :: Char -> String -> [String]
split c s = T.unpack <$> (T.split (== c) (T.pack s))

splitAny :: String -> String -> [String]
splitAny cs s = T.unpack <$> (T.split (\x -> (not . elem x) cs) (T.pack s))

splitMap :: Char -> (String -> String) -> String -> String
splitMap c f s =
    let words' = split c s in
    intercalate [c] $ f <$> words'

replaceUrls :: String -> (String -> String) -> String
replaceUrls s f =
    splitMap ' ' inner s
    where inner :: String -> String
          inner =
              splitMap '"' (\x -> (if isUrl x then f else id) x)

hasUrl :: String -> Bool
hasUrl s =
    let words' = split ' ' s in
    let words'' = split '"' <$> words' in
    any (/= []) $ (filter isUrl) <$> words''

extractUrl :: String -> String
extractUrl s =
    let words' = words s in
    case filter isUrl words' of
        (x:_) -> x
        _ -> "!!Error extracting url!! (" ++ s ++ ")"


replaceChar :: Char -> String -> String -> String
replaceChar c r s =
    concat $ helper c s r
    where helper :: Char -> String -> String -> [String]
          helper c (x:xs) r =
              if c == x
              then r : helper c xs r
              else [x] : helper c xs r
          helper c [] r = [[]]


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

extractAction :: [SqlValue] -> (String, String)
extractAction (x:y:z:_) =
    let isAction = (fromSql z :: Int) == 1 in
    let name = fromSql x in
    let y' = (if isAction
             then (\x-> "<i>" ++ name ++ " " ++ x ++ "</i>")--fixme
             else id) $ fromSql y in
    (name, y')
extractAction _ = (default', default')

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
    logInfo (msg ++ ": " ++ whitespace ++ printf "%.3fs" s)
    return res

split3 :: [(a, b, c, d)] -> ([(a, b)], [(a, c)], [(a,d)])
split3 xs =
    helper xs ([], [], [])
    where helper ((w, x, y, z):rest) (ac1, ac2, ac3) =
              helper rest ((w, x) : ac1, (w, y) : ac2, (w, z) : ac3)
          helper [] (ac1, ac2, ac3) = (reverse ac1, reverse ac2, reverse ac3)


lower :: String -> String
lower = (<$>) toLower

(.:) = (.) . (.)
(.::) = (.:) . (.)
(.:::) = (.::) . (.)
