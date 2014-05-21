{-# LANGUAGE DoAndIfThenElse, BangPatterns, FlexibleInstances #-}
module IRCDB.Renderer where

import Control.Arrow
import Control.Applicative
import Data.List

class Printable a where
    print' :: a -> String

instance Printable Int where
    print' = show

instance Printable String where
    print' = id


formatUserTimes :: [(String, Int, Int, Int, Int, Int, String)] -> String
formatUserTimes times =
    let formatted :: [String]
        formatted = formatTime <$> times in
    tag "table" $ concat formatted
    where formatTime :: (String, Int, Int, Int, Int, Int, String) -> String
          formatTime (user, w, x, y, z, total, message) =
            let rect = (makeRectScript user w x y z) in
            tag "tr" $ (td "20%" user)
                    ++ (td "10%" $ (makeCanvas user 100 16) ++ rect)
                    ++ (td "10%" $ show total)
                    ++ (td "60%" message)

makeCanvas :: String -> Int -> Int -> String
makeCanvas name width height =
    concat [ "<canvas id=\""
           , name
           , "\" width=\""
           , show width
           , "\" height=\""
           , show height
           , "\"></canvas>"
           ]

makeRectScript :: String
               -> Int
               -> Int
               -> Int
               -> Int
               -> String
makeRectScript name w x y z =
    let vals = [show name] ++ (show <$> [w, x, y, z]) in
    tag "script" $ makeCall "drawBar" vals

makeCall :: String -> [String] -> String
makeCall f args =
    let fmt = (intercalate ", " args)  in
    concat $ [f, "("] ++ [fmt] ++ [");"]

simpleFormat :: Show a => (String, a) -> String
simpleFormat (user, num) = user ++ ": " ++ show num

formatList :: Show a => [(String, a)] -> [String]
formatList = liftA simpleFormat

makeList :: [String] -> String
makeList xs = concat $ tag "p" <$> xs

withHeading :: String -> (String -> String)
withHeading h = (++) (tag "h2" h)

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (x, y) = (f x, f y)

headerTable :: Printable a => String -> (String, String) -> [(String, a)] -> String
headerTable h s xs =
    let mapped = (second print') <$> xs in
    withHeading h $ simpleTable ((pairMap (tag "b") s):mapped)

makeFile :: String -> String -> [String] -> String
makeFile x file scripts =
    let scriptSrc src = genTag "script" [("language", "javascript"), ("src", src)] "" in
    let css = voidTag "link" [("href",file),("rel", "stylesheet"), ("type", "text/css")] in
    let s :: [String]
        s = scriptSrc <$> scripts in
    tag "html" $ tag "head" (css ++ (concat $ s)) ++ tag "body" x

simpleTable :: Printable a => [(String,a)] -> String
simpleTable xs = tag "table" $ concat $ format <$> xs
    where format (s, y) = tag "tr" $ td "20%" s ++ td "80%" (print' y)


td :: String -> (String -> String)
td width =  genTag "td" [("width", width)]

linkify :: String -> String
linkify s = "<a href=\"" ++ s ++ "\">" ++ s ++ "</a>"

propToString :: (String,String) -> String
propToString (k, v) = k ++ "=\"" ++ v ++ "\" "

genTag :: String -> [(String,String)] -> String -> String
genTag t props c =
    let props' = foldl(\acc kv -> acc ++ propToString kv) "" props in
    concat ["<", t, " ", props', ">\n", c, "\n</", t, ">"]

voidTag :: String -> [(String,String)] -> String
voidTag t props =
    let props' = foldl(\acc kv -> acc ++ propToString kv) "" props in
    concat ["<", t, " ", props', ">\n"]

tag :: String -> String -> String
tag s c = genTag s [] c

