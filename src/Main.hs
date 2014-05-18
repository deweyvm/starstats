
import IRCDB.DB


main :: IO ()
main = do
    doAction Repopulate
    doAction Generate



