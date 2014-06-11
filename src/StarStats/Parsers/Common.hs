module StarStats.Parsers.Common where

import Control.Applicative((<*>), (<$>), (*>), (<*))
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity
import Data.Time.LocalTime

import StarStats.Time

type Name = String
type Contents = String

data DbParseError = DbParseError String -- line attempted to parse
                                 String -- error message
    deriving Show


type DLParser = String -> Either DbParseError [DataLine]



data DataLine = Message Time Int Name Contents
              | Nick Time Name Name
              | Kick Time Name Name Contents
              | Topic Time Name Contents
              | Join Time Name
              | Part Time Name Contents
              | Quit Time Name Contents
              | Mode Time Contents
              | Notice Time String
              | Invite Time String
              | Day (Month,DayOfMonth) Time
              | Date LocalTime
              | Close LocalTime
              | Open LocalTime
              | Bad Contents
    deriving (Show)

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

word :: Parser String
word = many (noneOf " ")

parseInt :: Parser Int
parseInt = fromInteger <$> integer

eatLine :: Parser String
eatLine = manyTill anyChar eof
