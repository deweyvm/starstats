{-# LANGUAGE DoAndIfThenElse, BangPatterns, FlexibleInstances, ExistentialQuantification, ImpredicativeTypes #-}
module StarStats.Renderer where

import Control.Arrow
import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Text.Printf
import System.Random
import System.IO.Unsafe
import StarStats.DB.Utils
import Data.IORef
data TimeBar = TimeBar String Int Int Int Int

toTimeBars :: [(String, Int, Int, Int, Int)] -> [(String, TimeBar)]
toTimeBars = ((\(user, w, x, y, z) -> (user, TimeBar user w x y z)) <$>)


instance Ord TimeBar where
    (TimeBar x _ _ _ _) `compare` (TimeBar y _ _ _ _) = x `compare` y

instance Eq TimeBar where
    (TimeBar x _ _ _ _) == (TimeBar y _ _ _ _) = x == y


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
    let rowData = getHeadingWidth <$> cs in
    Row $ (\(s, w) -> (tag "b" s, w)) <$> rowData


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
    let assemble' :: Name -> [(String, Width)]
        assemble' u = zip (find' u <$> maps) ws in
    let rows = Row . assemble' <$> us in
    hr : rows

formatTable :: Heading
            -> [Name]
            -> Heading
            -> Width
            -> [Column]
            -> String
formatTable h ns nh nw cs =
    let nameCol = Column (M.fromList $ zip ns ns) nh nw in
    let cs' = nameCol : cs in
    let rows = rowify ns cs' in
    let formatCell (s, w) = td (printf "%d%%" w) s in
    let formatRow (Row xs) = tr $ concat $ formatCell <$> xs in
    if length rows == 1
    then ""
    else withHeading h $ tag "table" $ concat $ formatRow <$> rows


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
    if length values == 0
    then ""
    else withHeading h $ canvas ++ (tag "script" $ (makeCall "drawGraph" vals))


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

headerTable :: Print a => String -> String -> String -> [(String, a)] -> String
headerTable h c1 c2 xs =
    if length xs == 0
        then ""
        else let s = (c1, c2) in
             let mapped = (second print') <$> xs in
             withHeading h $ simpleTable ((pairMap (tag "b") s):mapped)

makeFile :: String -> String -> String -> [String] -> String
makeFile x file head' scripts =
    let scriptSrc src = genTag "script" [("language", "javascript"), ("src", src)] "" in
    let css = voidTag "link" [("href",file),("rel", "stylesheet"), ("type", "text/css")] in
    let s :: [String]
        s = scriptSrc <$> scripts in
    tag "html" $ tag "head" (css ++ (concat $ s) ++ head') ++ tag "body" x

simpleTable :: Print a => [(String,a)] -> String
simpleTable xs = tag "table" $ concat $ format <$> xs
    where format (s, y) = tr $ td "30%" s ++ td "70%" (print' y)

{-# NOINLINE counter #-}
counter :: IORef Int
counter = unsafePerformIO $ newIORef 0

{-# NOINLINE makeExpandBox #-}
makeExpandBox :: String -> String
makeExpandBox x = unsafePerformIO $ do
    i <- readIORef counter
    writeIORef counter (i+1)
    let id' = "A" ++ (show i)
    let div' = genTag "div" [("class", "overflowtest")]
    let label' = genTag "label" [("for", id')]
    let inputTag = voidTag "input" [("id", id'), ("type", "checkbox")]
    return $ div' (inputTag ++ label' (tag "div" x))

td :: String -> (String -> String)
td width x =   (genTag "td" [("width", width)] (makeExpandBox x))

tr :: String -> String
tr = tag "tr"

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


linkLinks :: String -> String
linkLinks s = replaceUrls s (\x -> genTag "a" [("href", x)] x)
