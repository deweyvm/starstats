{-# LANGUAGE DoAndIfThenElse, BangPatterns #-}
module IRCDB.Renderer where

import Control.Applicative
import Debug.Trace
formatTimes :: (String, Int, Int, Int, Int, Int, String) -> String
formatTimes (user, w, x, y, z, total, message) =
    tag "table" $ tag "tr" $ (tag "td" user)
                          ++ (tag "td" $ show w)
                          ++ (tag "td" $ show x)
                          ++ (tag "td" $ show y)
                          ++ (tag "td" $ show z)
                          ++ (tag "td" $ (makeCanvas user))
                          ++ (tag "td" $ show total)
                          ++ (tag "td" message)

makeCanvas :: String -> String
makeCanvas name = "<canvas id=\"" ++ name ++ "\" width=\"578\" height=\"200\"></canvas>"

--makeRectScript :: String -> String
--makeRectScript name = "<script>\
--            \  var canvas = document.getElementById('" ++ name ++ "');\
--            \  var context = canvas.getContext('2d');\
--            \  context.beginPath();\
--            \  context.rect(0, 0, 32, 16);\
--            \  context.fillStyle = 'yellow';\
--            \  context.fill();\
--            \  context.lineWidth = 7;\
--            \  context.strokeStyle = 'black';\
--            \  context.stroke();\
--            \</script>"

simpleFormat :: Show a => (String, a) -> String
simpleFormat (user, num) = user ++ ": " ++ show num

formatList :: Show a => [(String, a)] -> [String]
formatList = liftA simpleFormat

makeList :: [String] -> String
makeList xs = tag "ul" $ concat $ tag "li" <$> xs

withHeading :: String -> (String -> String)
withHeading h = (++) (tag "h2" h)

makeFile :: String -> String
makeFile x = tag "html" $ tag "head" "" ++ tag "body" x


tag :: String -> String -> String
tag s c = concat ["<", s, ">\n", c, "\n</", s, ">"]
