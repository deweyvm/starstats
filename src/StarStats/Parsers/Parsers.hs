module StarStats.Parsers.Parsers where

import StarStats.Parsers.Common
import qualified StarStats.Parsers.Irssi as I
import qualified StarStats.Parsers.XChat as X

data ParserType = Irssi
                | XChat

getParser :: ParserType -> DLParser
getParser Irssi = I.parseLine
getParser XChat = X.parseLine
