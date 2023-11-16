import Text.Trifecta
import Data.Char
import Control.Applicative

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int
data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
                   deriving (Eq, Show)

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

pint :: Parser Int
pint = digitToInt <$> parseDigit

makeInt :: [Int] -> Int
makeInt = (foldl (\acc x -> 10 * acc + x) 0)

parseNPA :: Parser NumberingPlanArea
parseNPA = (try (makeInt <$> (count 3 pint))) <|> (try (makeInt <$> (between (symbol "(") (symbol ")") (count 3 pint))))

parseExchange :: Parser Exchange
parseExchange = makeInt <$> (count 3 pint)

parseLN :: Parser LineNumber
parseLN = makeInt <$> (count 4 pint)

parsePhoneAux :: Parser PhoneNumber
parsePhoneAux = PhoneNumber <$> parseNPA <* (skipOptional (char '-' <|> space)) <*> parseExchange <* (skipOptional (char '-')) <*> parseLN

parsePhone :: Parser PhoneNumber
parsePhone = (try ((string "1-") >> parsePhoneAux)) <|> parsePhoneAux
