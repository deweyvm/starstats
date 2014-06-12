module StarStats.Parsers.ZNC where

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
            <|> parseMessage

parsePrefix :: String -> Parser String
parsePrefix s = symbol "---" *> symbol s *> symbol ":"


parseTimeChange :: Parser DataLine
parseTimeChange = try (Open <$> parseLogDate)
              <|> (Close <$> parseLogDate)

parseLogDate :: Parser LocalTime
parseLogDate = do
    time <- parseFullTime <* parsePrefix "log"
                          <* parseNick --started/ended
                          <* parseNick --channelname
                          <* symbol "/"
    date <- parseDate
    return $ makeTime date time



guessYear :: Int -> Integer
guessYear year
    --irc didnt exist before the 80s i dont think...
    | year < 80 = fromIntegral $ 2000 + year
    | otherwise = fromIntegral $ 1900 + year

parseDate :: Parser (Integer, Int, Int)
parseDate = (,,) <$> (guessYear <$> (parseInt <* symbol "."))
                 <*> (parseInt <* symbol ".")
                 <*> parseInt

parseInvite :: Parser DataLine
parseInvite = Invite <$> parseTime
                     <*> ((symbol "!") *> eatLine)

parseNotice :: Parser DataLine
parseNotice = Notice <$> parseTime
                     <*> ((symbol "-") *> eatLine)

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
         <*> (parsePrefix "join" *> parseNick <* eatLine)

parseQuit :: Parser DataLine
parseQuit =
    Quit <$> parseTime
         <*> (parsePrefix "quit" *> parseNick)
         <*> (symbol "(" *> many (noneOf ")") <* (symbol ")"))

parsePart :: Parser DataLine
parsePart =
    Part <$> parseTime
         <*> (parsePrefix "part" *> parseNick)
         <*> (symbol "left #" *> parseNick *> return "")



parseMode :: Parser DataLine
parseMode = Mode <$> parseTime
                 <*> (parsePrefix "mode" *> eatLine)


--00:00:00 --- topic: set to 'this is the topic' by user

parseTopic :: Parser DataLine
parseTopic = Topic <$> parseTime
                   <*> (parsePrefix "topic")
                   <*> (symbol "set to '" *> eatLine)

parseKick :: Parser DataLine
parseKick = Kick <$> parseTime
                 <*> (parsePrefix "kick" *> parseNick)
                 <*> (symbol "was kicked by" *> parseNick)
                 <*> eatLine --todo, strip parens

parseNick :: Parser Name
parseNick = many (noneOf " ") <* whiteSpace


parseNickChange :: Parser DataLine
parseNickChange = Nick <$> parseTime
                       <*> (parsePrefix "nick" *> parseNick)
                       <*> (symbol "->" *> parseNick)

parseAction :: Parser DataLine
parseAction = Message <$> parseTime
                      <*> return 1
                      <*> (symbol "*" *> parseNick)
                      <*> eatLine

parseMessage :: Parser DataLine
parseMessage = Message <$> parseTime
                       <*> return 0
                       <*> (string "<" *> many (noneOf ">") <* symbol ">")
                       <*> eatLine


parseFullTime :: Parser (Int,Int,Int)
parseFullTime = (,,) <$> (parseInt <* symbol ":")
                     <*> (parseInt <* symbol ":")
                     <*> parseInt
parseTime :: Parser Time
parseTime = do
    (h, m, s) <- parseFullTime
    return (h, m)


parseLine :: String -> Either DbParseError [DataLine]
parseLine s =
    case parse parseDataLine "" s of
        Left err -> Left (DbParseError s (show err))
        Right success -> Right [success]
