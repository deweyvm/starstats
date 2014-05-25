{-# LANGUAGE DoAndIfThenElse, BangPatterns, FlexibleInstances #-}
module IRCDB.Renderer where

import Control.Arrow
import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Text.Printf


class Defaultable a where
    default' :: a

instance Defaultable Double where
    default' = 0

instance Defaultable Int where
    default' = 0

instance Defaultable [Char] where
    default' = ""

data TimeBar = TimeBar String Int Int Int Int

class Printable a where
    print' :: a -> String

instance Printable Int where
    print' = show

instance Printable Double where
    print' = printf "%.2f"

instance Printable String where
    print' = id

instance Printable TimeBar where
    print' (TimeBar user w x y z) =
        (makeCanvas user 100 16) ++ (makeRectScript user w x y z)

type Heading = String
type Name = String
type Width = Int
data Column a = Column (M.Map Name a) Heading Width
data Row a = Row [(String, Width)]

toColumn :: Ord a => [(String, a)] -> Heading -> Width -> Column a
toColumn xs h w = Column (M.fromList xs) h w

--may be possible to pass width directly
getHeadingWidth :: [Column a] -> ([Heading], Width)
getHeadingWidth cs =
    let width = case cs of
                    ((Column _ _ w):_) -> w
                    _ -> error "bad" in
    ((\(Column _ h _) -> h) <$> cs, width)

toRow :: Printable a => [(Name, [a], Width)] -> [Row a]
toRow xs = (Row . doMap) <$> xs
    where doMap (name, elts, width) = zip (name : (print' <$> elts)) (repeat width)

makeHeadingRow :: [Column a] -> Row a
makeHeadingRow cs =
    let (hs, w) = getHeadingWidth cs in
    Row $ zip hs (repeat w)

getMap :: Column a -> M.Map String a
getMap (Column m _ _) = m

getWidth :: Column a -> Width
getWidth (Column _ _ w) = w

rowify :: (Ord a, Defaultable a, Printable a)
       => [Name]
       -> [Column a]
       -> [Row a]
rowify us cs =
    let hr = makeHeadingRow cs in
    let maps = getMap <$> cs in
    let ws = getWidth <$> cs in
    let find' u m = print' $ fromMaybe default' (M.lookup u m) in
    let assemble :: Name -> [(String, Width)]
        assemble u = zip (find' u <$> maps) ws in
    let rows = Row . assemble <$> us in
    hr : rows

formatTable :: (Ord a, Defaultable a, Printable a)
            => [Name]
            -> [Column a]
            -> String
formatTable ns cs =
    let rows = rowify ns cs in
    let formatCell (s, w) = td (printf "%d%%" w) s in
    let formatRow (Row xs) = tr $ concat $ formatCell <$> xs in
    concat $ formatRow <$> rows

formatUserTimes :: [(String, TimeBar, Int, String)] -> String
formatUserTimes times =
    let addRow w x y z = tr $ (td "20%" w)
                           ++ (td "10%" x)
                           ++ (td "10%" y)
                           ++ (td "60%" z) in
    let formatTime :: (String, TimeBar, Int, String) -> String
        formatTime (user, bar, total, message) =
            addRow user (print' bar) (show total) message in
    let formatted :: [String]
        formatted = formatTime <$> times in
    let heading = addRow "User" "Activity" "Total" "Random Message" in
    tag "table" $ heading ++ (concat formatted)


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
    where format (s, y) = tr $ td "20%" s ++ td "80%" (print' y)


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

