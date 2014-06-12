module StarStats.Parsers.ChatZilla where

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
parseDataLine = parseChatLine

parseChatLine :: Parser [DataLine]
parseChatLine = try (return <$> parseStatus)
            <|> (try parseMessage)
            <|> (return <$> parseBad)


parseStatus :: Parser DataLine
parseStatus = try parseQuit
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
         <*> (symbol "-->|" *> parseNick <* symbol "("
                                         <* many (noneOf ")")
                                         <* symbol ") has joined"
                                         <* eatLine)
parseQuit :: Parser DataLine
parseQuit =
    Quit <$> parseTime
         <*> (symbol "|<--" *> parseNick <* symbol "has left")
         <*> eatLine

parsePart :: Parser DataLine
parsePart =
    Quit <$> parseTime
         <*> (symbol "<--|" *> parseNick <* symbol "has left")
         <*> eatLine

parseBad :: Parser DataLine
parseBad =
    Bad <$> (parseTime *> badInner *> eatLine)
    where badInner =
                (try (symbol "*" *> parseNick *> symbol "*")
             <|> try (symbol "=-= User mode for")
             <|> try (symbol "[INFO]")
             <|> try (symbol "[QUIT]")
             <|> try (symbol "[ERROR]")
             <|> try (symbol "=-= YOU are now known as")
             <|> try (symbol "-->| YOU" *> symbol "(" *> many (noneOf ")") *> symbol ") have joined")
             <|> try (symbol "<--| YOU" *> symbol "(" *> many (noneOf ")") *> symbol ") have left")
             <|> try (symbol "=-= YOU" *> symbol "(" *> many (noneOf ")") *> symbol ") have been booted")
             <|> try (symbol "=-= Topic for #" *> parseNick
                                                      *> symbol "is")
             <|> try (symbol "=-= Topic for #" *> parseNick
                                                      *> symbol "was set by")
             <|> try (symbol "===")
             <|> try (symbol "---")
             <|> try (symbol ">" *> parseNick *> symbol "<"))


parseMode :: Parser DataLine
parseMode =
    Mode <$> parseTime
         <*> (symbol "=-= Mode #" *> parseNick
                                  *> eatLine)

trimTopic :: String -> String
trimTopic s =
    let s' = (drop 2 s) in
    take (length s' - 2) s'

parseTopic :: Parser DataLine
parseTopic = do
    Topic <$> parseTime
          <*> (symbol "=-=" *> parseNick)
          <*> (symbol "has changed the topic to " *> (trimTopic <$> eatLine))

parseKick :: Parser DataLine
parseKick =
    Kick <$> parseTime
         <*> (symbol "=-=" *> parseNick)
         <*> (symbol "was booted from #" *> parseNick
                                         *> symbol "by "
                                         *> parseNick)
         <*> eatLine

parseNick :: Parser Name
parseNick = many (noneOf "<*# ") <* whiteSpace

parseNickChange :: Parser DataLine
parseNickChange =
    Nick <$> parseTime
         <*> (symbol "=-=" *> parseNick)
         <*> (symbol "is now known as" *> parseNick)

parseAction :: Parser DataLine
parseAction =
    Message <$> parseTime
            <*> return 1
            <*> (symbol "* " *> parseNick)
            <*> eatLine

parseDate :: Parser LocalTime
parseDate = do
    time <- symbol "[" *> many (noneOf "]") <* symbol "]"
    return $ fromMaybe (error $ "Bad time format: " ++ time) (chatZillaParseDate time)

parseMessage :: Parser [DataLine]
parseMessage = do
    date <- parseDate
    type' <- return 0
    name <- (string "<" *> parseName <* symbol ">")
    contents <- parseContents
    return [Date date, Message (localTimeToTime date) type' name contents]


parseTime :: Parser Time
parseTime = do
    date <- parseDate
    return $ localTimeToTime date

parseName :: Parser Name
parseName = manyTill (noneOf "*") (lookAhead (symbol ">"))


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
