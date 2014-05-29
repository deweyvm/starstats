import System.Environment
import Control.Applicative
import IRCDB.DB.Driver


main :: IO ()
main = do
    args <- getArgs
    let actions = if elem "-g" args then [Repopulate, Generate] else [Generate]
    sequence_ $ doAction <$> actions



