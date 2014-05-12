{-# LANGUAGE DoAndIfThenElse #-}
module IRCDB.Renderer where
import System.Directory

templateFile :: String
templateFile = "template"

readTemplate :: IO String
readTemplate = do
    exists <- doesFileExist templateFile
    if not exists
    then error $ "file \"" ++ templateFile ++ "\" not found"
    else readFile templateFile
