module IRCDB.Time where

import qualified Data.Time.Format as Time
import Data.Time.Lens
import Data.Time.LocalTime
import Data.Time.Calendar
import System.Locale

type Time = (Int, Int)

tryMaybe :: (a -> Maybe b) -> (c -> Maybe b) -> a -> c -> Maybe b
tryMaybe f g x y = case f x of
    Just t -> Just t
    Nothing -> g y

stringToLocalTime :: String -> Maybe LocalTime
stringToLocalTime s =
    tryMaybe timeStringToLocalTime dateStringToLocalTime s s

timeStringToLocalTime :: String -> Maybe LocalTime
timeStringToLocalTime s =
    Time.parseTime defaultTimeLocale "%a %b %e %H:%M:%S %Y" s

dateStringToLocalTime :: String -> Maybe LocalTime
dateStringToLocalTime s =
    Time.parseTime defaultTimeLocale "%a %b %e %Y" s

setHoursMinutes :: LocalTime -> Time -> LocalTime
setHoursMinutes t (h, m) = setL hours h $ setL minutes m t

subHours :: LocalTime -> (Int->Int) -> LocalTime
subHours t h = (modL hours h) t

deleteSeconds :: LocalTime -> LocalTime
deleteSeconds t = (setL seconds 0) t

anyTime :: LocalTime
anyTime = (LocalTime (fromGregorian 0 0 0) (TimeOfDay 0 0 0))
