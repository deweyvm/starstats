{-# LANGUAGE DoAndIfThenElse #-}
module StarStats.Log.Log where

import System.IO.Unsafe
import System.IO.UTF8
import System.IO (stderr, hFlush)
data LogLevel = None
              | Error
              | Warning
              | Info
              | Verbose
              | All
              deriving (Ord, Eq, Show)

logLevel :: LogLevel
logLevel = Error

format :: LogLevel -> String -> String
format l s = concat ["[", shows l, "]: ", s]
    where shows All = "ALL"
          shows Error = "ERR"
          shows Warning = "WAR"
          shows Info = "INF"
          shows Verbose = "VER"
          shows None = "NON"

safeLog :: LogLevel -> String -> IO ()
safeLog l s = do
    if l > logLevel
    then return ()
    else do hPutStrLn stderr (format l s)
            hFlush stderr

logAll :: String -> IO ()
logAll s = safeLog All s

logVerbose :: String -> IO ()
logVerbose s = safeLog Verbose s

logInfo :: String -> IO ()
logInfo s = safeLog Info s

logWarning :: String -> IO ()
logWarning s = safeLog Warning s

logError :: String -> IO ()
logError s = safeLog Error s

{-# NOINLINE unsafeLog #-}
unsafeLog :: LogLevel -> String -> a -> a
unsafeLog l s x = unsafePerformIO $ do
    safeLog l s
    return x
