import Text.Trifecta
import Control.Applicative
import Data.Char

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

pint :: Parser Integer
pint = (toInteger . digitToInt) <$> parseDigit

base10Integer :: Parser Integer
base10Integer = (foldl (\acc x -> 10 * acc + x) 0) <$> many pint
