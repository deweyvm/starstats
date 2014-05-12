module IRCDB.Time where


import qualified Data.Time.Format as Time
import Data.Time.Lens
import Data.Time.LocalTime
import Data.Time.Calendar

import System.Locale
type Time = (Int, Int)

stringToLocalTime :: String -> Maybe LocalTime
stringToLocalTime s = Time.parseTime defaultTimeLocale  "%a %b %e %H:%M:%S %Y" s

setHoursMinutes :: LocalTime -> Time -> LocalTime
setHoursMinutes t (h, m) = setL hours h $ setL minutes m t

anyTime :: LocalTime
anyTime = (LocalTime (fromGregorian 0 0 0) (TimeOfDay 0 0 0))
