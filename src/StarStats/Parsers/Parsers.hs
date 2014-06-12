module StarStats.Parsers.Parsers where

import StarStats.Parsers.Common
import qualified StarStats.Parsers.Irssi as I
import qualified StarStats.Parsers.XChat as X
import qualified StarStats.Parsers.ChatZilla as C

data ParserType = Irssi
                | XChat
                | ChatZilla

getParser :: ParserType -> DLParser
getParser Irssi = I.parseLine
getParser XChat = X.parseLine
getParser ChatZilla = C.parseLine
