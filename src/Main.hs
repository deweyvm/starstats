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

    let sinfo = ServerInfo (optDriverName opts)
                           (optDbName opts)

    doAction mode sinfo
    logInfo "Shutting down gracefully"
