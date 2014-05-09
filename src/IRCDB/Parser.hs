module IRCDB.Parser where

import Control.Applicative((<*>), (<$>), (*>), (<*))
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity
import Debug.Trace

type Name = String
type Contents = String
type Time = (Int, Int)
type Date = String

data DataLine = Message Time Bool Name Contents
              | Action Time Name Contents
              | Status Time String
              | Notice Time String
              | Invite Time String
              | Day Date
              | Close Date
              | Open Date
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

parseOp :: Parser Bool
parseOp = (try (symbol "+") *> return True)
      <|> (whiteSpace *> return False)

parseName :: Parser Name
parseName = manyTill anyChar (lookAhead (symbol ">"))

eatLine :: Parser String
eatLine = many (noneOf "\n") <* symbol "\n" <?> "whole line"

parseContents :: Parser Contents
parseContents = eatLine


parseDateString :: Parser Date
parseDateString = eatLine <?> "date string"

--parseLog :: Parser Log
--parseLog =  Log <$> manyTill parseDataLine eof

data Thing = Thing [Either ParseError DataLine]

thingLength :: Thing -> Int
thingLength (Thing ls) = length ls


parseFile :: String -> Either (String, ParseError) DataLine
parseFile line =
    case parse parseDataLine "" line of
        Left err -> Left(line, err)
        Right success -> Right success
