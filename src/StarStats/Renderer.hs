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
import StarStats.Utils
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
        let uname = makeUserTag user in
        (makeCanvas uname 100 16) ++ (makeRectScript uname w x y z)


type Heading = String
type Name = String
type Width = String
data Column = Column (M.Map Name String) Heading Width
data Row = Row [(String, Width)]

toColumn :: [(String, String)] -> Heading -> Width -> Column
toColumn xs h w = Column (M.fromList (mapFst lower <$> xs)) h w

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
    let find' u m = fromMaybe "~~~There was an error finding that message~~~" (M.lookup u m) in
    let assemble' :: Name -> [(String, Width)]
        assemble' u = zip (find' u <$> maps) ws in
    let rows = Row . assemble' <$> us in
    hr : rows

formatTable :: Heading
            -> String
            -> [Name]
            -> Heading
            -> Width
            -> [Column]
            -> Bool
            -> Maybe String
formatTable h desc ns nh nw cs b =
    let ns' = (lower <$> ns) in
    let nameCol = toColumn (zip ns' ns) nh nw in
    let cs' = nameCol : cs in
    let rows = rowify ns' cs' in
    let formatCell (s, w) = td b w s in
    let formatRow (Row xs) = tr $ concat $ formatCell <$> xs in
    if length rows == 1
    then Nothing
    else Just $ linkHeader Table h desc $ tag "table" $ concat $ formatRow <$> rows

makeUserTag :: String -> String
makeUserTag s = "user-user-user-user-user-" ++ s

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
    let vals = [show (name)] ++ (show <$> [w, x, y, z]) in
    tag "script" $ makeCall "drawBar" vals


showPair :: (String,Double) -> String
showPair (s, i) = "[" ++ show s ++ "," ++ show i ++ "]"

pairToPercent :: [(a,Int)] -> [(a,Double)]
pairToPercent xs =
    let sum' = fromIntegral $ sum $ snd <$> xs :: Double in
    (\(x, i) -> (x, (fromIntegral $ 100 * i) / sum')) <$> xs


makeDonutGraph :: String -> String -> String -> String -> [(String,Int)] -> Maybe String
makeDonutGraph type' h desc canvasName xs =
    let hours = pairToPercent xs in
    let vals = "[" ++ (intercalate ", " $ showPair <$> hours) ++ "]" in
    let s = makeCall type' [ show canvasName
                           , vals
                           ] in
    let tag' = divId canvasName ""in
    if length vals == 0
    then Nothing
    else Just $ linkHeader DonutGraph h desc $ tag' ++  genTag "script" [("type", "text/javascript")] s

makeDonut = makeDonutGraph "donut"

makeHalfDonut = makeDonutGraph "halfDonut"

showInt :: Int -> String
showInt 0 = "null"
showInt x = show x

makeLine :: String -> String -> String -> String -> [(String,Int)] -> Maybe String
makeLine h desc canvasName label xs =
    let labels = (intercalate ", " $ (show.fst) <$> xs) in
    let vals = (intercalate ", " $ (showInt.snd) <$> xs) in
    let s = makeCall "line" [ show canvasName
                            , "[" ++ vals ++ "]"
                            , "[" ++ labels ++ "]"
                            , show label
                            ] in
    let tag' = divId canvasName ""in
    if length vals == 0
    then Nothing
    else Just $ linkHeader LineGraph h desc $ tag' ++  genTag "script" [("type", "text/javascript")] s

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

withHeading3 :: String -> String -> Section -> (String -> String)
withHeading3 h desc s x =
    let divv = divClass "myhr" in
    let spann = spanClass "myhr-inner" in
    let spanDesc = spanClass "myhr-desc" in
    let h' = "&nbsp;&nbsp;" ++ h ++ "" in
    divClass (sectionString s) $ (divv (spann (h' ++ spanDesc (hoverBox desc)))) ++ x

section :: [String] -> String
section [] = ""
section xs = divClass "section" $ (unlines xs)

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (x, y) = (f x, f y)


data Section = DonutGraph | LineGraph | Table


sectionString :: Section -> String
sectionString DonutGraph = "graph-element-donut"
sectionString LineGraph = "graph-element-line"
sectionString Table = "table-element"

linkHeader :: Section -> String -> String -> String -> String
linkHeader sec h desc s =
    let tagname = hyphenate h in
    let href = genTag "a" [("id", tagname), ("href", "#" ++ tagname)] in
    (withHeading3 (href h) desc sec $ s)

hoverBox :: String -> String
hoverBox desc = spanClass "htip" (tag "div" ("[?]&nbsp;" ++ (divClass "hwrap" $ divClass "hbox" desc)))

makeFile :: String -> String -> String -> [String] -> String
makeFile x file head' scripts =
    let favicon = voidTag "link" [ ("href", "/starstats/favicon.ico?v=1.1")
                                 , ("rel", "shortcut icon")
                                 ] in
    let scriptSrc src = genTag "script" [ ("language", "javascript")
                                        , ("src", src)] "" in
    let css = voidTag "link" [ ("href", file)
                             , ("rel", "stylesheet")
                             , ("type", "text/css")
                             ] in
    let s :: [String]
        s = scriptSrc <$> scripts in
    tag "html" $ tag "head" (css ++ (concat $ s) ++ head' ++ favicon) ++ tag "body" (divId "container" x)

simpleTable :: Print a => String -> String -> Bool -> [(String,a)] -> String
simpleTable w0 w1 b xs = tag "table" $ concat $ format <$> xs
    where format (s, y) = tr $ td b w0 s ++ td b w1 (print' y)

{-# NOINLINE counter #-}
counter :: IORef Int
counter = unsafePerformIO $ newIORef 0

{-# NOINLINE makeExpandBox #-}
makeExpandBox :: Bool -> String -> String
makeExpandBox b x = unsafePerformIO $ do
    i <- readIORef counter
    writeIORef counter (i+1)
    let id' = "A" ++ (show i)
    let div' = divClass "overflowbox"
    let label' = genTag "label" [("for", id')]
    let tags = [ ("id", id')
               , ("type", "checkbox")
               , ("autocomplete", "off")
               ] ++ if b then [("checked", "")] else []
    let inputTag = voidTag "input" tags
    return $ div' (inputTag ++ label' (divClass "testtest" x))

td :: Bool -> String -> (String -> String)
td b width x = (genTag "td" [("width", width)] (makeExpandBox b x))

tr :: String -> String
tr = tag "tr"

divId :: String -> (String -> String)
divId id' = genTag "div" [("id", id')]

divClass :: String -> (String -> String)
divClass class' = genTag "div" [("class", class')]

spanClass :: String -> (String -> String)
spanClass class' = genTag "span" [("class", class')]

propToString :: (String,String) -> String
propToString (k, v) = k ++ "=\"" ++ v ++ "\" "

genTag :: String -> [(String,String)] -> String -> String
genTag t props c =
    let props' = foldl (\acc kv -> acc ++ propToString kv) "" props in
    concat ["<", t, " ", props', ">\n", c, "\n</", t, ">"]

voidTag :: String -> [(String,String)] -> String
voidTag t props =
    let props' = foldl (\acc kv -> acc ++ propToString kv) "" props in
    concat ["<", t, " ", props', "/>\n"]

tag :: String -> String -> String
tag s c = genTag s [] c
