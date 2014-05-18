{-# LANGUAGE DoAndIfThenElse #-}
module IRCDB.Renderer where

import Control.Applicative



makeList :: [String] -> String
makeList xs = tag "ul" $ concat $ tag "li" <$> xs

withHeading :: String -> (String -> String)
withHeading h = (++) (tag "h2" h)

makeFile :: String -> String
makeFile x = tag "html" $ tag "head" "" ++ tag "body" x


tag :: String -> String -> String
tag s c = concat ["<", s, ">\n", c, "\n</", s, ">"]
