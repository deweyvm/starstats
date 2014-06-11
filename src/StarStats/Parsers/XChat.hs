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

import StarStats.Parsers.Common

parseDataLine :: Parser DataLine
parseDataLine = try (parseTimeChange) <|> parseChatLine

parseChatLine :: Parser DataLine
parseChatLine = try parseStatus
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
    Bad <$> (parseTime *> (try (symbol "* Disconnected (No such device or address)")
                       <|> try (symbol "* ChanServ gives channel operator status to ")
                       <|> try (symbol "* ChanServ gives channel half-operator status to")
                       <|> try (symbol "* You are now known as")
                       <|> try (symbol "* Topic for #")
                       <|> try (symbol "-NickServ- ")
                       <|> try (parseNick *> symbol "plugin unloaded")
                       <|> symbol "* Now talking on #") *> eatLine)



parseMode :: Parser DataLine
parseMode = Mode <$> parseTime
                 <*> (symbol "*" *> parseNick
                                 *> symbol "sets mode"
                                 *> eatLine)

parseTopic :: Parser DataLine
parseTopic = Topic <$> parseTime
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
parseNickChange = Nick <$> parseTime
                       <*> (symbol "*" *> parseNick)
                       <*> (symbol "is now known as" *> parseNick)

parseAction :: Parser DataLine
parseAction = Message <$> parseTime
                      <*> return 1
                      <*> (symbol "*" *> parseNick)
                      <*> eatLine

parseMessage :: Parser DataLine
parseMessage = Message <$> parseTime
                       <*> return 0
                       <*> (string "<" *> parseName <* symbol ">")
                       <*> parseContents


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

parseLine :: String -> Either DbParseError DataLine
parseLine s =
    if length s == 0
    then Right (Bad "")
    else case parse parseDataLine "" s of
             Left err -> Left (DbParseError s (show err))
             Right success -> Right success
