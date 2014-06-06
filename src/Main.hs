{-# LANGUAGE DoAndIfThenElse #-}

import System.Environment
import Control.Applicative
import System.IO
import StarStats.DB.Driver
import StarStats.Watcher
import StarStats.DB.Utils

main :: IO ()
main = do
    args <- getArgs
    let hasArg s = elem s args
    let driver = args !! 0
    let chanName = args !! 1
    let action = case () of
                  ()| hasArg "-rp" -> Repopulate (args !! 2)
                    | hasArg "-rv" -> Recover (args !! 2)
                    | hasArg "-rd" -> Read
                    | hasArg "-g" -> Generate


    let sinfo = ServerInfo driver chanName
    doAction action sinfo
