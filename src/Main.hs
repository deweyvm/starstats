{-# LANGUAGE DoAndIfThenElse #-}
import Database.HDBC
import Database.HDBC.ODBC
import Control.Applicative
import System.Directory
import IRCDB.Parser

configFile :: String
configFile = "config"

readConfig :: IO String
readConfig = do
    exists <- not <$> doesFileExist configFile
    if exists
    then
        error "file 'config' not found"
    else do
        config <- lines <$> readFile configFile
        return $ processConfig config
    where processConfig (c:_) = c
          processConfig     _ = error "file 'config' is empty"


processOne :: IConnection con => con -> Either (Int, String, String) DataLine -> IO ()
processOne _ (Left (ln, s, err)) = do
    putStrLn ("Line " ++ show ln)
    print s
    print err
processOne con (Right l) = insert l con

insert :: IConnection con => DataLine -> con -> IO ()
insert (Message time op name msg) con = do
    prepared <- prepare con "INSERT INTO text (name, flags, text, time) VALUES (?,?,?,?);"
    let sqlName = toSql name
    let sqlOp = toSql op
    let sqlMsg = toSql msg
    let sqlTime = (toSql.fst) time
    rows <- execute prepared [sqlName, sqlOp, sqlMsg, sqlTime]
    return ()

insert _ _ = return ()
main :: IO ()
main = do
    logfile <- readConfig
    contents <- lines <$> readFile logfile
    --putStrLn contents
    --print $ length $
    let parsed = parseLine <$> zip [1..] contents



    let connectionString =  "DSN=name32;Driver={MySQL ODBC 5.3 ANSI Driver};Server=localhost;Port=3306;Database=testdb;User=root;Password=password;Option=3;"
    let ioconn = connectODBC connectionString
    conn <- ioconn
    sequence_ $ processOne conn <$> parsed
    commit conn


