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

data DataLine = Message Date Time Bool Name Contents
              | Status Date String
    deriving (Show)

data Day = Day [DataLine] deriving (Show)

data Log = Log [Day] deriving (Show)

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
symbol  = P.symbol lexer

parseDataLine :: Date -> Parser DataLine
parseDataLine date = (parseStatus date) <|> parseMessage date

parseMessage :: Date -> Parser DataLine
parseMessage date = Message <$> (return date)
                            <*> parseTime
                            <*> (symbol "<" *> parseOp)
                            <*> (parseName <* symbol ">")
                            <*> parseContents

parseStatus :: Date -> Parser DataLine
parseStatus date = Status <$> (return date)
                          <*> (symbol "-!-" *> eatLine)

parseInt :: Parser Int
parseInt = fromInteger <$> integer

parseTime :: Parser Time
parseTime = (,) <$> (parseInt <* symbol ":") <*> parseInt

parseOp :: Parser Bool
parseOp = (try (symbol "+") *> return True) <|> (whiteSpace *> return False)

parseName :: Parser Name
parseName = identifier

eatLine :: Parser String
eatLine = many $ noneOf "\n"

parseContents :: Parser Contents
parseContents = do
    line <- eatLine
    return $ traceShow ("LINE: " ++ line) line

parseDateString :: Parser Date
parseDateString = eatLine

parseDate :: Parser Date
parseDate = symbol "--- Day changed" *> parseDateString

parseWholeDay :: Parser Day
parseWholeDay = do
    date <- parseDate
    parseDay date

parseDay :: Date -> Parser Day
parseDay date = Day <$> manyTill (parseDataLine date) (lookAhead (symbol "---"))

parseLog :: Parser Log
parseLog = do
    date <- symbol "--- Log opened" *> parseDateString
    first <- parseDay date
    rest <- manyTill parseWholeDay eof
    return $ Log (traceShow date first:rest)


parseFile :: String -> Either ParseError Log
parseFile = parse parseLog ""
