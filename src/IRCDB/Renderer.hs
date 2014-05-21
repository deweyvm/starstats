{-# LANGUAGE DoAndIfThenElse, BangPatterns, FlexibleInstances #-}
module IRCDB.Renderer where
import Control.Arrow
import Control.Applicative

class Printable a where
    print' :: a -> String

instance Printable Int where
    print' = show

instance Printable String where
    print' = id


formatTimes :: [(String, Int, Int, Int, Int, Int, String)] -> String
formatTimes times =
    let formatted :: [String]
        formatted = formatTime <$> times in
    tag "table" $ concat formatted
    where formatTime :: (String, Int, Int, Int, Int, Int, String) -> String
          formatTime (user, w, x, y, z, total, message) =

            let rect = (makeRectScript user w x y z) in
            tag "tr" $ (td "20%" user)
                    ++ (td "10%" $ (makeCanvas user) ++ rect)
                    ++ (td "10%" $ show total)
                    ++ (td "60%" message)
makeCanvas :: String -> String
makeCanvas name = "<canvas id=\"" ++ name ++ "\" width=\"100\" height=\"16\"></canvas>"


makeRectScript :: String
               -> Int
               -> Int
               -> Int
               -> Int
               -> String
makeRectScript name w x y z = "<script>\
            \  var canvas = document.getElementById('" ++ name ++ "');\
            \  var context = canvas.getContext('2d');\
            \  context.beginPath();\
            \  context.fillStyle = '#FF0000';\
            \  context.fillRect(0, 0, " ++ show w ++ ", 16);\
            \  context.fillStyle = '#FF00FF';\
            \  context.fillRect(" ++ show w ++ ", 0, " ++ show x ++ ", 16);\
            \  context.fillStyle = '#0000FF';\
            \  context.fillRect(" ++ show (w + x) ++ ", 0, " ++ show y ++ ", 16);\
            \  context.fillStyle = '#00FFFF';\
            \  context.fillRect(" ++ show (w + x + y) ++ ", 0, " ++ show z ++ ", 16);\
            \</script>"

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

makeFile :: String -> String -> String
makeFile x file =
    let css = "<LINK href=\"" ++ file ++ "\" rel=\"stylesheet\" type=\"text/css\">" in
    tag "html" $ tag "head" css ++ tag "body" x

simpleTable :: Printable a => [(String,a)] -> String
simpleTable xs = tag "table" $ concat $ format <$> xs
    where format (s, y) = tag "tr" $ td "20%" s ++ td "80%" (print' y)

tag :: String -> String -> String
tag s c = genTag s [] c

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
