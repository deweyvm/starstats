{-# LANGUAGE DoAndIfThenElse #-}
module IRCDB.Renderer where
import Data.List


makeList :: [String] -> String
makeList xs = "<ul><li>" ++ (intercalate "</li><li>" xs) ++ "</li></ul>"

withHeading :: String -> (String -> String)
withHeading h = (++) ("<h1>" ++ h ++ "</h1>")

makeFile :: String -> String
makeFile x = "<html><head></head><body>" ++ x ++ "</body></html>"
