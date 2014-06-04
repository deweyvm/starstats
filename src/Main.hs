{-# LANGUAGE DoAndIfThenElse #-}

import System.Environment
import Control.Applicative
import IRCDB.DB.Driver
import IRCDB.Watcher

main :: IO ()
main = do
    args <- getArgs
    if elem "-w" args
    then let file = args !! 0
         let repop = args !! 1 == "-rp"
         watch file repop
    else let driver = args !! 0
         let chanName = args !! 1
         let actions = if elem "-p" args then [Repopulate, Generate] else [Generate]
         sequence_ $ doAction driver chanName <$> actions
