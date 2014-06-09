module StarStats.Args where

import System.Console.GetOpt
import System.Exit
import System.Environment
import System.IO.UTF8
import System.IO (stderr)

import StarStats.Parsers.Common
import StarStats.Parsers.Parsers
import StarStats.DB.Utils
import StarStats.Log.Log


data Options = Options { optDbName :: String
                       , optDriverName :: String
                       , optMode :: Maybe ActionType
                       , optParseType :: Maybe ParserType
                       , optLog :: Maybe String
                       }

startOptions :: Options
startOptions = Options { optDbName = ""
                       , optDriverName = ""
                       , optMode = Nothing
                       , optParseType = Nothing
                       , optLog = Nothing
                       }


options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "" ["db"]
        (ReqArg
            (\arg opt -> return opt { optDbName = arg })
            "DBNAME")
        "Database name"
    , Option "" ["driver"]
        (ReqArg
            (\arg opt -> return opt { optDriverName = arg })
            "DRIVER")
        "The ODBC driver to use"
    , Option "" ["log"]
        (OptArg
            (\arg opt -> return opt { optLog = arg })
            "FILE")
        "The logfile to read"
    , Option "" ["generate"]
        (NoArg
            (\opt -> return opt { optMode = Just GenerateT }))
        "(Mode) Generate a webpage"
    , Option "" ["repopulate"]
        (NoArg
            (\opt -> return opt { optMode = Just RepopulateT }))
        "(Mode) Repopulate the database"
    , Option "" ["recover"]
        (NoArg
            (\opt -> return opt { optMode = Just RecoverT }))
        "(Mode) Recover the database from the last known message"
    , Option "" ["read"]
        (NoArg
            (\opt -> return opt { optMode = Just ReadT }))
        "(Mode) Read data lines from stdin"
    , Option "" ["init"]
        (NoArg
            (\opt -> return opt {optMode = Just InitializeT }))
        "(Mode) Initialize a blank database"
    , Option "" ["logtype"]
        (OptArg
            (\arg opt -> do type' <- loadParseType arg
                            return opt { optParseType = Just type' })
            "TYPE")
        "Log file format"
    ]

rawToAction :: Options -> IO Action
rawToAction opts = do
    let action = optMode opts
    case action of
       Just ReadT -> loadRead opts
       Just RecoverT -> loadRecover opts
       Just GenerateT -> return Generate
       Just InitializeT -> return Initialize
       Just RepopulateT -> loadRepopulate opts
       Nothing -> do logError "Must specify a mode"
                     exitFailure

loadRepopulate :: Options -> IO Action
loadRepopulate opts = do
    case (optParseType opts, optLog opts) of
        (Just t, Just l) -> return $ Repopulate (getParser t) l
        (_, Nothing) -> do logError "A logtype must be specified for this mode"
                           exitFailure
        (Nothing, _) -> do logError "A log must be specified for this mode"
                           exitFailure


loadRecover :: Options -> IO Action
loadRecover opts = do
    case (optParseType opts, optLog opts) of
        (Just t, Just l) -> return $ Recover (getParser t) l
        (_, Nothing) -> do logError "A logtype must be specified for this mode"
                           exitFailure
        (Nothing, _) -> do logError "A log must be specified for this mode"
                           exitFailure


loadRead :: Options -> IO Action
loadRead opts =
    case optParseType opts of
        Just t -> return $ Read $ getParser t
        Nothing -> do logError "Must specify logtype for this option"
                      exitFailure

loadParseType :: Maybe String -> IO ParserType
loadParseType s = do
    case s of
        Just "irssi" -> return $ Irssi
        Just "xchat" -> return $ XChat
        _       -> do prog <- getProgName
                      hPutStrLn stderr ("Options for logtype are irssi|xchat" ++ usageInfo prog options)
                      exitFailure

assertDefined :: String -> Maybe String -> IO String
assertDefined _ (Just s) = return s
assertDefined msg Nothing = do
    prog <- getProgName
    hPutStrLn stderr ("Failed assertion " ++ msg ++  usageInfo prog options)
    exitFailure

getOpts :: IO Options
getOpts = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    foldl (>>=) (return startOptions) actions
