{-# LANGUAGE DoAndIfThenElse, BangPatterns #-}

import GHC.IO.Encoding
import StarStats.Args
import StarStats.DB.Driver
import StarStats.DB.Utils
import StarStats.Log.Log

main :: IO ()
main = do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8
    logInfo "Setting output encoding"
    opts <- getOpts
    mode <- rawToAction opts
    let doReset = optReset opts

    let sinfo = ServerInfo (optDriverName opts)
                           (optDbName opts)
    if (optShowHelp opts)
    then do printUsage
            exitWith ExitSuccess
    else return ()
    doAction doReset mode sinfo
    logInfo "Shutting down gracefully"
