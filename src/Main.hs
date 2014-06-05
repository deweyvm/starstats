{-# LANGUAGE DoAndIfThenElse #-}

import System.Environment
import Control.Applicative
import System.IO
import StarStats.DB.Driver
import StarStats.Watcher

main :: IO ()
main = do

    args <- getArgs
    let driver = args !! 0
    let chanName = args !! 1
    if elem "-w" args
    then do let file = args !! 2
            let repop = elem "-rp" args
            let recover = elem "-rv" args
            watch file repop recover driver chanName
    else do let actions = if elem "-p" args
                              then [Repopulate, Generate]
                              else [Generate]
            sequence_ $ doAction driver chanName <$> actions
