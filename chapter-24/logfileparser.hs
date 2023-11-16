import Text.Trifecta
import Control.Applicative
import Data.Char

type Year = Int
type Month = Int
type Day = Int
type Hour = Int
type Min = Int
type Activity = String
data LogTime = LogTime Year Month Day Hour Min deriving (Show)
data LogEntry = LogEntry LogTime Activity deriving (Show)

logBlock = "#2025-11-07 -- first day\n09:00 programming -- some comment\n12:00 lunch\n13:00 job search"
logBlock2 = "#2025-10-03\n08:30 breakfast\n09:00 studing -- akita videos\n13:00 lunch\n14:00 programming -- haskell"
logFile = logBlock ++ "\n" ++ logBlock2


parseComment :: Parser String
parseComment = token (string "--" >> ((try (manyTill (notChar '\n') eof)) <|> manyTill (notChar '\n') (char '\n')))

pint :: Parser Int
pint = fromInteger <$> integer

parseMakeLogTime :: Parser (Int -> Int -> LogTime)
parseMakeLogTime = char '#' >> skipOptional whiteSpace >> LogTime <$> pint <* char '-' <*> pint <* char '-' <*> pint <* skipOptional parseComment

parseLog :: Parser (Hour, Min, Activity)
parseLog = (,,) <$> pint <* char ':' <*> pint <* whiteSpace <*> ((try (manyTill (notChar '\n') parseComment)) <|> (try (manyTill (notChar '\n') (char '\n'))) <|> (manyTill (notChar '\n') eof))

parseOneDayLogBlock :: Parser [LogEntry]
parseOneDayLogBlock = 
 do
   skipOptional parseComment
   makeLogTime <- parseMakeLogTime
   partialLogEntryList <- many parseLog
   let logEntryList = foldr (\(hour, min, activity) acc -> (LogEntry (makeLogTime hour min) activity):acc) [] partialLogEntryList
   return logEntryList

parseLogFile :: Parser [[LogEntry]]
parseLogFile = manyTill parseOneDayLogBlock eof
