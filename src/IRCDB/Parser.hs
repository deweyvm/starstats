module IRCDB.Parser where

import Control.Applicative((<*>), (<$>), (*>), (<*))
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity
import Data.Time.LocalTime
import IRCDB.Time

type Name = String
type Contents = String

data DataLine = Message Time Bool Name Contents
              | Action Time Name Contents
              | Status Time String
              | Notice Time String
              | Invite Time String
              | Day LocalTime
              | Close LocalTime
              | Open LocalTime
    deriving (Show)

data Log = Log [DataLine] deriving (Show)

logLength :: Log -> Int
logLength (Log ls) = length ls

lexer :: P.GenTokenParser String a Identity
lexer = P.makeTokenParser $ emptyDef

integer :: Parser Integer
integer = P.integer lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

lexeme :: Parser String -> Parser String
lexeme = P.lexeme lexer

identifier :: Parser String
identifier = P.identifier lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

parseDataLine :: Parser DataLine
parseDataLine = try (parseTimeChange) <|> parseChatLine

parseChatLine :: Parser DataLine
parseChatLine = try parseStatus
            <|> try parseNotice
            <|> try parseAction
            <|> try parseInvite
            <|> parseMessage


parseTimeChange :: Parser DataLine
parseTimeChange = try (Day <$> (symbol "--- Day changed" *> parseDateString))
              <|> try (Close <$> (symbol "--- Log closed" *> parseDateString))
              <|> (Open <$> (symbol "--- Log opened" *> parseDateString))

parseInvite :: Parser DataLine
parseInvite = Invite <$> parseTime
                     <*> ((symbol "!") *> eatLine)

parseNotice :: Parser DataLine
parseNotice = Notice <$> parseTime
                     <*> ((symbol "-") *> eatLine)

parseStatus :: Parser DataLine
parseStatus = Status <$> parseTime
                     <*> (symbol "-!-" *> eatLine)
                     <?> "status"

parseAction :: Parser DataLine
parseAction = Status <$> parseTime
                     <*> (symbol "*" *> eatLine)
                     <?> "action"

parseMessage :: Parser DataLine
parseMessage = Message <$> parseTime
                       <*> (symbol "<" *> parseOp)
                       <*> (parseName <* symbol ">")
                       <*> parseContents
                       <?> "message"

parseInt :: Parser Int
parseInt = fromInteger <$> integer
                       <?> "integer"

parseTime :: Parser Time
parseTime = (,) <$> (parseInt <* symbol ":")
                <*> parseInt
                <?> "time"
-- ~ channel owner
-- @ op
-- + voice
-- % half op
parseOp :: Parser Bool
parseOp = (try (oneOf "@+%~") *> return True)
      <|> (whiteSpace *> return False)

parseName :: Parser Name
parseName = manyTill anyChar (lookAhead (symbol ">"))

eatLine :: Parser String
eatLine = manyTill anyChar eof <?> "whole line"

parseContents :: Parser Contents
parseContents = eatLine


get :: Maybe LocalTime -> LocalTime
get = maybe (anyTime) id

parseDateString :: Parser LocalTime
parseDateString = (get . stringToLocalTime) <$> eatLine <?> "date string"

parseLine :: (Int, String) -> Either (Int, String, String) DataLine
parseLine (ln, s) =
    case parse parseDataLine "" s of
        Left err -> Left (ln, s, show err)
        Right success -> Right success
