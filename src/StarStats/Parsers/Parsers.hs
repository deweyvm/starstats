module StarStats.Parsers.Parsers where

import StarStats.Parsers.Common
import qualified StarStats.Parsers.Irssi as I
import qualified StarStats.Parsers.XChat as X
import qualified StarStats.Parsers.ChatZilla as C
import qualified StarStats.Parsers.ZNC as Z

data ParserType = Irssi
                | XChat
                | ChatZilla
                | ZNC

getParser :: ParserType -> DLParser
getParser Irssi = I.parseLine
getParser XChat = X.parseLine
getParser ChatZilla = C.parseLine
getParser ZNC = Z.parseLine
