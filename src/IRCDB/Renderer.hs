{-# LANGUAGE DoAndIfThenElse #-}
module IRCDB.Renderer where

import Control.Applicative

formatTimes :: (String, Int, Int, Int, Int, Int) -> String
formatTimes (user, w, x, y, z, total) =
    concat [ user
           , ": "
           , show w
           , " "
           , show x
           , " "
           , show y
           , " "
           , show z
           , " "
           , show total
           ]

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
