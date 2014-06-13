module StarStats.Parsers.ZNC where

import Control.Applicative((<*>), (<$>), (*>), (<*))
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity
import Data.Time.LocalTime
import Data.Maybe
import Data.List(intersperse)
import StarStats.Time
import StarStats.Parsers.Common

parseDataLine :: Parser DataLine
parseDataLine = try (parseTimeChange) <|> parseChatLine

parseChatLine :: Parser DataLine
parseChatLine = try parseAction
            <|> try parseStatus
            <|> try parseInvite
            <|> try parseMessage
            <|> try parseBad
            <|> parseTopic

parsePrefix :: String -> Parser String
parsePrefix s = symbol "--- " *> symbol s *> symbol ":"


parseTimeChange :: Parser DataLine
parseTimeChange = try (Open <$> parseLogDate)
              <|> (Close <$> parseLogDate)

parseLogDate :: Parser LocalTime
parseLogDate = do
    time <- parseFullTime <* parsePrefix "log"
                          <* parseNick --started/ended
                          <* many (noneOf "/") --channelname
                          <* char '/'
    date <- parseDate
    return $ makeTime date time

--- quit: Overflow ("

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


parseStatus :: Parser DataLine
parseStatus = try parseQuit
          <|> try parsePart
          <|> try parseJoin
          <|> try parseMode
          <|> try parseNickChange
          <|> parseKick

parseBad :: Parser DataLine
parseBad =
    Bad <$> (parseTime *> badInner *> eatLine)
    where badInner = try (parsePrefix "topic" *> char '\'' *> return "")
                 <|> try (parsePrefix "topic" *> symbol "set by ")
                 <|> try (symbol "***")
                 <|> try (char '-' *> noneOf "-" *> return "")
                 <|> parsePrefix "names"
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


extractLongestQuote :: String -> (String,String)
extractLongestQuote xs =
    (\(x, y) -> (reverse x, y)) $ helper xs []
    where helper (x:xs) acc =
            if elem '\'' xs
            then helper xs (x:acc)
            else (acc, xs)

stripBy :: String -> String
stripBy (' ':'b':'y':' ':xs) = xs
stripBy s = s

--00:00:00 --- topic: set to 'this is the topic' by user
parseTopic :: Parser DataLine
parseTopic = do
    time <- parseTime <* parsePrefix "topic" <* symbol "set to"
    contents <- char '\'' *> eatLine
    let (topic, rest) = extractLongestQuote contents
    let name = stripBy rest
    return $ Topic time name topic

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
