module StarStats.Parsers.Irssi where

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
parseChatLine = try parseAction
            <|> try parseStatus
            <|> try parseNotice
            <|> try parseInvite
            <|> try parseMessage
            <|> parseBad


parseBad :: Parser DataLine
parseBad = Bad <$> (try (parseTime *> symbol "-!-" *> eatLine)
                    <|> (parseTime *> symbol "-" *> eatLine)
                   )
parseTimeChange :: Parser DataLine
parseTimeChange = try (Date <$> (symbol "--- Day changed" *> parseDateString))
              <|> try (Close <$> (symbol "--- Log closed" *> parseDateString))
              <|> (Open <$> (symbol "--- Log opened" *> parseDateString))

parseInvite :: Parser DataLine
parseInvite = Invite <$> parseTime
                     <*> ((symbol "!") *> eatLine)

parseNotice :: Parser DataLine
parseNotice = Notice <$> parseTime
                     <*> (between (symbol "[") (symbol "]") (many (noneOf "]")) *> eatLine)

parseStatus :: Parser DataLine
parseStatus = try parseQuit
          <|> try parsePart
          <|> try parseJoin
          <|> try parseMode
          <|> try parseNickChange
          <|> try parseKick
          <|> parseTopic

parseJoin :: Parser DataLine
parseJoin =
    Join <$> parseTime
         <*> (symbol "-!-" *> parseNick <* symbol "["
                                        <* many (noneOf "]")
                                        <* symbol "] has joined"
                                        <* eatLine)

parseLeave :: (Time -> Name -> Contents -> DataLine) -> String -> Parser DataLine
parseLeave ctor s =
    ctor <$> parseTime
         <*> (symbol "-!-" *> parseNick <* many (noneOf "]") <* symbol ("] " ++ s))
         <*> eatLine

parseQuit :: Parser DataLine
parseQuit = parseLeave Quit "has quit"

parsePart :: Parser DataLine
parsePart = parseLeave Part "has left"

parseMode :: Parser DataLine
parseMode = Mode <$> parseTime
                 <*> (symbol "-!- mode/#" *> word *> eatLine)



parseTopic :: Parser DataLine
parseTopic = Topic <$> parseTime
                   <*> (symbol "-!-" *> parseNick)
                   <*> (symbol "changed the topic of #"
                           *> parseNick
                           *> symbol "to: "
                           *> eatLine)

parseKick :: Parser DataLine
parseKick = Kick <$> parseTime
                 <*> (symbol "-!-" *> parseNick)
                 <*> (symbol "was kicked from #" *> word *> symbol " by " *> parseNick <* whiteSpace)
                 <*> eatLine

parseNick :: Parser Name
parseNick = many (noneOf " ") <* whiteSpace

parseNickChange :: Parser DataLine
parseNickChange = Nick <$> parseTime
                       <*> (symbol "-!-" *> parseNick)
                       <*> (symbol "is now known as" *> parseNick)

parseAction :: Parser DataLine
parseAction = Message <$> parseTime
                      <*> return 1
                      <*> (symbol "*" *> parseNick)
                      <*> eatLine

parseMessage :: Parser DataLine
parseMessage = Message <$> parseTime
                       <*> return 0
                       <*> (string "<" *> oneOf " +~@%&" *> parseName <* symbol ">")
                       <*> parseContents


parseTime :: Parser Time
parseTime = (,) <$> (parseInt <* symbol ":")
                <*> parseInt

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
    case parse parseDataLine "" s of
        Left err -> Left (DbParseError s (show err))
        Right success -> Right [success]
