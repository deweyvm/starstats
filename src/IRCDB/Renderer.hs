{-# LANGUAGE DoAndIfThenElse #-}
module IRCDB.Renderer where
import Data.List
import System.Directory


makeList :: [String] -> String
makeList xs = "<ul><li>" ++ (intercalate "</li><li>" xs) ++ "</li></ul>"


makeFile :: String -> String
makeFile x = "<html><head></head><body>" ++ x ++ "</body></html>"
