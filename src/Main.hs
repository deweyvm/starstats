import System.Environment
import Control.Applicative
import IRCDB.DB.Driver


main :: IO ()
main = do
    args <- getArgs
    let driver = args !! 0
    let actions = if elem "-p" args then [Repopulate, Generate] else [Generate]
    sequence_ $ doAction driver <$> actions
    print "Finished"


