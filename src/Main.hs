{-# LANGUAGE DoAndIfThenElse, BangPatterns #-}

import Prelude hiding(log)
import Data.Maybe
import System.Environment
import System.Exit
import Control.Applicative
import System.IO
import System.Console.GetOpt
import GHC.IO.Encoding hiding (close)
import StarStats.DB.Driver
import StarStats.Watcher
import StarStats.DB.Utils
import StarStats.Log.Log


data Options = Options { optDbName :: String
                       , optDriverName :: String
                       , optChannelName :: Maybe String
                       , optServerName :: Maybe String
                       , optMode :: Action
                       }
startOptions :: Options
startOptions = Options { optDbName = ""
                       , optDriverName = ""
                       , optChannelName = Nothing
                       , optServerName = Nothing
                       , optMode = Initialize
                       }


options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "" ["db"]
        (ReqArg
            (\arg opt -> return opt { optDbName = arg })
            "DBNAME")
        "Database name"
    , Option "" ["server"]
        (OptArg
           (\arg opt -> return opt { optServerName = arg })
           "SERVER")
        "Server name"
    , Option "" ["channel"]
        (OptArg
           (\arg opt -> return opt { optChannelName = arg })
           "CHANNEL")
        "Channel name"
    , Option "" ["driver"]
        (ReqArg
            (\arg opt -> return opt { optDriverName = arg })
            "DRIVER")
        "The ODBC driver to use"
    , Option "" ["generate"]
        (NoArg
            (\opt -> return opt { optMode = Generate }))
        "Generate a webpage"
    , Option "" ["repopulate"]
        (OptArg
            (\arg opt -> return opt { optMode = Repopulate ("" `fromMaybe` arg) })
            "FILE")
        "Repopulate the database"
    , Option "" ["recover"]
        (OptArg
            (\arg opt -> return opt { optMode = Recover ("" `fromMaybe` arg) })
            "FILE")
        "Recover the database from the last known message"
    , Option "" ["read"]
        (NoArg
            (\opt -> return opt { optMode = Read }))
        "Read data lines from stdin"
    , Option "" ["init"]
        (NoArg
            (\opt -> return opt {optMode = Initialize }))
        "Initialize a blank database"
    ]


assertDefined :: String -> Maybe String -> IO String
assertDefined _ (Just s) = return s
assertDefined msg Nothing = do
    prg <- getProgName
    hPutStrLn stderr ("Failed assertion " ++ msg ++  usageInfo prg options)
    exitFailure

main :: IO ()
main = do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8
    logInfo "Setting output encoding"
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions

    chanName <- assertDefined "Channel Name" $ optChannelName opts
    serverName <- assertDefined "Server Name" $ optServerName opts

    let sinfo = ServerInfo (optDriverName opts)
                           (chanName)
                           (serverName)
                           (optDbName opts)
    doAction (optMode opts) sinfo
    logInfo "Shutting down gracefully"
