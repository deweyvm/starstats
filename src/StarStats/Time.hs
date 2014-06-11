module StarStats.Time where

import qualified Data.Time.Format as Time
import Data.Time.Lens
import Data.Time.LocalTime
import Data.Time.Calendar
import System.Locale
type Hour = Int
type Minute = Int
type Month = Int
type DayOfMonth = Int
type Time = (Hour, Minute)

tryMaybe :: (a -> Maybe b) -> (c -> Maybe b) -> a -> c -> Maybe b
tryMaybe f g x y = case f x of
    Just t -> Just t
    Nothing -> g y

localTimeToTime :: LocalTime -> Time
localTimeToTime date =
    let time = localTimeOfDay date in
    (todHour time, todMin time)

stringToLocalTime :: String -> Maybe LocalTime
stringToLocalTime s =
    tryMaybe timeStringToLocalTime dateStringToLocalTime s s

timeStringToLocalTime :: String -> Maybe LocalTime
timeStringToLocalTime s =
    Time.parseTime defaultTimeLocale "%a %b %e %H:%M:%S %Y" s

timeStringToDayHourMinute :: String -> Maybe (Month, DayOfMonth, Hour, Minute)
timeStringToDayHourMinute s = do
    localTime <- Time.parseTime defaultTimeLocale "%b %d %H:%M:%S %Y" s
    let (_, month, dayOfMonth) = toGregorian $ localDay localTime
    let time = localTimeOfDay localTime
    let hour = todHour time
    let minute = todMin time
    return (month, dayOfMonth, hour, minute)

getYear :: LocalTime -> Integer
getYear t = getL year t

setYear :: LocalTime -> Integer -> LocalTime
setYear t y = setL year y t

getMonth :: LocalTime -> Int
getMonth t = getL month t

dateStringToLocalTime :: String -> Maybe LocalTime
dateStringToLocalTime s =
    Time.parseTime defaultTimeLocale "%a %b %e %Y" s

setHoursMinutes :: LocalTime -> Time -> LocalTime
setHoursMinutes t (h, m) = setL hours h $ setL minutes m t


setMonthsDays :: LocalTime -> (Month,DayOfMonth) -> LocalTime
setMonthsDays t (m, d) = setL month m $ setL day d t

subHours :: LocalTime -> (Int->Int) -> LocalTime
subHours t h = (modL hours h) t

deleteSeconds :: LocalTime -> LocalTime
deleteSeconds t = (setL seconds 0) t

anyTime :: LocalTime
anyTime = (LocalTime (fromGregorian 0 0 0) (TimeOfDay 0 0 0))
