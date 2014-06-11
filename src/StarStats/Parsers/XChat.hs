module StarStats.Parsers.XChat where

import Control.Applicative((<*>), (<$>), (*>), (<*))
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity
import Data.Time.LocalTime
import Data.Maybe
import StarStats.Time
import StarStats.DB.Utils
import StarStats.Parsers.Common

parseDataLine :: Parser [DataLine]
parseDataLine = try (return <$> parseTimeChange) <|> parseChatLine

parseChatLine :: Parser [DataLine]
parseChatLine = try (return <$> parseStatus)
            <|> parseMessage


parseTimeChange :: Parser DataLine
parseTimeChange = try (Close <$> (symbol "**** ENDING LOGGING AT" *> parseDateString))
                  <|> (Open <$> (symbol "**** BEGIN LOGGING AT" *> parseDateString))

parseStatus :: Parser DataLine
parseStatus = try parseBad
          <|> try parseQuit
          <|> try parsePart
          <|> try parseJoin
          <|> try parseMode
          <|> try parseNickChange
          <|> try parseKick
          <|> try parseTopic
          <|> parseAction

parseJoin :: Parser DataLine
parseJoin =
    Join <$> parseTime
         <*> (symbol "*" *> parseNick <* symbol "("
                                      <* many (noneOf ")")
                                      <* symbol ") has joined")

parseQuit :: Parser DataLine
parseQuit =
    Quit <$> parseTime
         <*> (symbol "*" *> parseNick <* symbol "has quit")
         <*> eatLine

parsePart :: Parser DataLine
parsePart =
    Part <$> parseTime
         <*> (symbol "*" *> parseNick <* symbol "("
                                      <* many (noneOf ")")
                                      <* symbol ") has left")
         <*> return ""



parseBad :: Parser DataLine
parseBad =
    Bad <$> (parseTime *> badInner *> eatLine)
    where specialAction s = (symbol "*" *> parseNick
                                        *> symbol s)
          badInner =
                (try (symbol "* Disconnected (No such device or address)")
            <|> try (specialAction "gives channel operator status to ")
            <|> try (specialAction "removes channel operator status from")
            <|> try (specialAction "gives channel half-operator status to")
            <|> try (specialAction "removes channel half-operator status from")
            <|> try (specialAction "removes voice from")
            <|> try (specialAction "gives voice to")
            <|> try (specialAction "removes ban on")
            <|> try (specialAction "sets ban on")
            <|> try (symbol "* You are now known as")
            <|> try (symbol "* Topic for #")
            <|> try (symbol "-NickServ- ")
            <|> try (parseNick *> symbol "plugin unloaded")
            <|> symbol "* Now talking on #")

parseMode :: Parser DataLine
parseMode =
    Mode <$> parseTime
         <*> (symbol "*" *> parseNick
                         *> symbol "sets mode"
                         *> eatLine)

parseTopic :: Parser DataLine
parseTopic =
    Topic <$> parseTime
          <*> (symbol "*" *> parseNick)
          <*> (symbol "has changed the topic to:" *> eatLine)

parseKick :: Parser DataLine
parseKick =
    let kick t kicker kickee reason = Kick t kickee kicker reason in
    kick <$> parseTime
         <*> (symbol "*" *> parseNick)
         <*> (symbol "has kicked" *> parseNick *> symbol " from " *> word <* whiteSpace)
         <*> eatLine

parseNick :: Parser Name
parseNick = many (noneOf "# ") <* whiteSpace

parseNickChange :: Parser DataLine
parseNickChange =
    Nick <$> parseTime
         <*> (symbol "*" *> parseNick)
         <*> (symbol "is now known as" *> parseNick)

parseAction :: Parser DataLine
parseAction =
    Message <$> parseTime
            <*> return 1
            <*> (symbol "*" *> parseNick)
            <*> eatLine

parseDay :: Parser (Month, DayOfMonth, Time)
parseDay = do
    month <- parseNick
    dayOfMonth <- many (noneOf " ") <* whiteSpace
    hour <- many (noneOf ":") <* symbol ":"
    minute <- many (noneOf ":") <* symbol ":"
    second <- many (noneOf " ") <* whiteSpace
    let s = concat [ month
                   , " "
                   , dayOfMonth
                   , " "
                   , hour
                   , ":"
                   , minute
                   , ":"
                   , second
                   , " 09" --arbitrary, this is ignored anyway.
                   ]
    case timeStringToDayHourMinute s of
        Just (w, x, y, z) -> return (w, x, (y, z))
        Nothing -> unexpected s

parseMessage :: Parser [DataLine]
parseMessage = do
    (month, day, time) <- parseDay
    type' <- return 0
    name <-(string "<" *> parseName <* symbol ">")
    contents <- parseContents
    return [Day (month, day) time, Message time type' name contents]


parseTime :: Parser Time
parseTime = (,) <$> (word *> symbol " " *> parseInt *> parseInt <* symbol ":")
                <*> (parseInt <* symbol ":" <* parseInt)

parseName :: Parser Name
parseName = manyTill anyChar (lookAhead (symbol ">"))


parseContents :: Parser Contents
parseContents = eatLine


get :: Maybe LocalTime -> LocalTime
get = fromMaybe anyTime

parseDateString :: Parser LocalTime
parseDateString = (get . stringToLocalTime) <$> eatLine

parseLine :: String -> Either DbParseError [DataLine]
parseLine s =
    if length s == 0
    then Right [Bad ""]
    else case parse parseDataLine "" s of
             Left err -> Left (DbParseError s (show err))
             Right success -> Right success
