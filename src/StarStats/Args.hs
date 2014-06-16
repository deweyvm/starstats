module StarStats.Args where

import Control.Applicative
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
                       , optReset :: Bool
                       , optShowHelp :: Bool
                       }

startOptions :: Options
startOptions = Options { optDbName = ""
                       , optDriverName = ""
                       , optMode = Nothing
                       , optParseType = Nothing
                       , optLog = Nothing
                       , optReset = False
                       , optShowHelp = False
                       }


options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "h?" ["help"]
        (NoArg
            (\opt -> do return opt { optShowHelp = True })
        "Print this message and exit"
    , Option "" ["db"]
        (ReqArg
            (\arg opt -> return opt { optDbName = "starstats_" ++ arg })
            "DBNAME")
        "Database name"
    , Option "" ["driver"]
        (ReqArg
            (\arg opt -> return opt { optDriverName = arg })
            "DRIVER")
        "The ODBC driver to use"
    , Option "" ["reset"]
        (NoArg
            (\opt -> return opt { optReset = True }))
        "Truncate database before performing the action"
    , Option "" ["log"]
        (OptArg
            (\arg opt -> return opt { optLog = arg })
            "FILE")
        "The logfile to read"
    , Option "" ["generate"]
        (NoArg
            (\opt -> return opt { optMode = Just GenerateT }))
        "(Mode) Generate a webpage"
    , Option "" ["insert"]
        (NoArg
            (\opt -> return opt { optMode = Just InsertT }))
        "(Mode) Insert a file's data into the database"
    , Option "" ["watch"]
        (NoArg
            (\opt -> return opt { optMode = Just WatchT }))
        "(Mode) Insert a file's data into the database then watch it for changes"
    , Option "" ["only-watch"]
        (NoArg
            (\opt -> return opt { optMode = Just OnlyWatchT }))
        "(Mode) (Debug) Only watch the given file for changes"
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

printUsage :: IO ()
printUsage = do
    prog <- getProgName
    hPutStrLn stderr $ usageInfo prog options


rawToAction :: Options -> IO Action
rawToAction opts = do
    let action = optMode opts
    case action of
       Just WatchT -> loadWatch opts Watch
       Just OnlyWatchT -> loadWatch opts OnlyWatch
       Just GenerateT -> return Generate
       Just InitializeT -> return Initialize
       Just InsertT -> loadInsert opts
       Nothing -> do logError "Must specify a mode"
                     printUsage
                     exitFailure

loadInsert :: Options -> IO Action
loadInsert opts = do
    case (optParseType opts, optLog opts) of
        (Just t, Just l) -> return $ Insert (getParser t) l
        (_, Nothing) -> do logError "A logtype must be specified for this mode"
                           printUsage
                           exitFailure
        (Nothing, _) -> do logError "A log must be specified for this mode"
                           printUsage
                           exitFailure


loadWatch :: Options -> (DLParser -> String -> Action) -> IO Action
loadWatch opts ctor = do
    case (optParseType opts, optLog opts) of
        (Just t, Just l) -> return $ ctor (getParser t) l
        (_, Nothing) -> do logError "A logtype must be specified for this mode"
                           printUsage
                           exitFailure
        (Nothing, _) -> do logError "A log must be specified for this mode"
                           printUsage
                           exitFailure


loadParseType :: Maybe String -> IO ParserType
loadParseType s = do
    case s of
        Just "irssi" -> return $ Irssi
        Just "xchat" -> return $ XChat
        Just "chatzilla" -> return $ ChatZilla
        Just "znc" -> return $ ZNC
        _       -> do logError "Options for logtype are irssi|xchat|chatzilla|znc"
                      printUsage
                      exitFailure

getOpts :: IO Options
getOpts = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    foldl (>>=) (return startOptions) actions
