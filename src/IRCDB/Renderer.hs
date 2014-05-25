{-# LANGUAGE DoAndIfThenElse, BangPatterns, FlexibleInstances, ExistentialQuantification, ImpredicativeTypes
 #-}
module IRCDB.Renderer where

import Control.Arrow
import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Text.Printf


class Default a where
    default' :: a

instance Default Double where
    default' = 0

instance Default Int where
    default' = 0

instance Default [Char] where
    default' = ""

data TimeBar = TimeBar String Int Int Int Int

instance Ord TimeBar where
    (TimeBar x _ _ _ _) `compare` (TimeBar y _ _ _ _) = x `compare` y

instance Eq TimeBar where
    (TimeBar x _ _ _ _) == (TimeBar y _ _ _ _) = x == y



class Print a where
    print' :: a -> String

instance Print Int where
    print' = show

instance Print Double where
    print' = printf "%.2f"

instance Print String where
    print' = id

instance Print TimeBar where
    print' (TimeBar user w x y z) =
        (makeCanvas user 100 16) ++ (makeRectScript user w x y z)

type Heading = String
type Name = String
type Width = Int
data Column = Column (M.Map Name String) Heading Width
data Row = Row [(String, Width)]

toColumn :: [(String, String)] -> Heading -> Width -> Column
toColumn xs h w = Column (M.fromList xs) h w

--may be possible to pass width directly
getHeadingWidth :: Column -> (Heading, Width)
getHeadingWidth = (,) <$> getHeading <*> getWidth

toRow :: [(Name, [String], Width)] -> [Row]
toRow xs = (Row . doMap) <$> xs
    where doMap (name, elts, width) = zip (name : (print' <$> elts)) (repeat width)

makeHeadingRow :: [Column] -> Row
makeHeadingRow cs =
    Row $ getHeadingWidth <$> cs

getMap :: Column -> M.Map String String
getMap (Column m _ _) = m

getWidth :: Column -> Width
getWidth (Column _ _ w) = w

getHeading :: Column -> Heading
getHeading (Column _ h _) = h
rowify :: [Name]
       -> [Column]
       -> [Row]
rowify us cs =
    let hr = makeHeadingRow cs in
    let maps = getMap <$> cs in
    let ws = getWidth <$> cs in
    let find' u m = print' $ fromMaybe default' (M.lookup u m) in
    let assemble :: Name -> [(String, Width)]
        assemble u = zip (find' u <$> maps) ws in
    let rows = Row . assemble <$> us in
    hr : rows

formatTable :: [Name]
            -> Heading
            -> Width
            -> [Column]
            -> String
formatTable ns nh nw cs =
    let nameCol = Column (M.fromList $ zip ns ns) nh nw in
    let cs' = nameCol : cs in
    let rows = rowify ns cs' in
    let formatCell (s, w) = td (printf "%d%%" w) s in
    let formatRow (Row xs) = tr $ concat $ formatCell <$> xs in
    tag "table" $ concat $ formatRow <$> rows


makeCanvas :: String -> Int -> Int -> String
makeCanvas name width height =
    genTag "canvas" [ ("id", name)
                    , ("width", show width)
                    , ("height", show height)
                    ] ""

makeRectScript :: String
               -> Int
               -> Int
               -> Int
               -> Int
               -> String
makeRectScript name w x y z =
    let vals = [show name] ++ (show <$> [w, x, y, z]) in
    tag "script" $ makeCall "drawBar" vals

makeTimeScript :: String -> [(Int,Int)] -> String
makeTimeScript h hours =
    let canvas = makeCanvas "timegraph" (24*24) 140 in
    let values :: [String]
        values = (show . snd) <$> hours in
    let fmt = (intercalate ", " values) in
    let vals = [show "timegraph"] ++ [("[" ++ fmt ++ "]")] in
    withHeading h $ canvas ++ (tag "script" $ (makeCall "drawGraph" vals))


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

headerTable :: Print a => String -> (String, String) -> [(String, a)] -> String
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

simpleTable :: Print a => [(String,a)] -> String
simpleTable xs = tag "table" $ concat $ format <$> xs
    where format (s, y) = tr $ td "30%" s ++ td "70%" (print' y)


td :: String -> (String -> String)
td width =  genTag "td" [("width", width)]

tr :: String -> String
tr = tag "tr"

linkify :: String -> String
linkify s = genTag "a" [("href", s)] s

propToString :: (String,String) -> String
propToString (k, v) = k ++ "=\"" ++ v ++ "\" "

genTag :: String -> [(String,String)] -> String -> String
genTag t props c =
    let props' = foldl (\acc kv -> acc ++ propToString kv) "" props in
    concat ["<", t, " ", props', ">\n", c, "\n</", t, ">"]

voidTag :: String -> [(String,String)] -> String
voidTag t props =
    let props' = foldl (\acc kv -> acc ++ propToString kv) "" props in
    concat ["<", t, " ", props', ">\n"]

tag :: String -> String -> String
tag s c = genTag s [] c

