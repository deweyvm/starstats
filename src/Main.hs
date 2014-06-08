{-# LANGUAGE DoAndIfThenElse, BangPatterns #-}

import Prelude hiding(log)
import System.Environment
import Control.Applicative
import System.IO
import GHC.IO.Encoding hiding (close)
import StarStats.DB.Driver
import StarStats.Watcher
import StarStats.DB.Utils
import StarStats.Log.Log

main :: IO ()
main = do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8
    logInfo "Setting output encoding"
    args <- getArgs
    let hasArg s = elem s args
    let driver = args !! 0
    let chanName = args !! 1
    let action = case () of
                  ()| hasArg "-rp" -> Repopulate (args !! 2)
                    | hasArg "-rv" -> Recover (args !! 2)
                    | hasArg "-rd" -> Read
                    | hasArg "-g" -> Generate
                    | hasArg "-i" -> Initialize
                    | otherwise -> error $ "Unknown args " ++ show args

    let sinfo = ServerInfo driver chanName
    doAction action sinfo
    logInfo "Shutting down gracefully"
