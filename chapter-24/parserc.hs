module LearnParsers where

import Text.Trifecta
import Control.Applicative

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

oneTwoThree :: Parser Char
oneTwoThree = char '1' >> char '2' >> char '3'

p123 :: Parser String
p123 = (string "123" <|> string "12" <|> string "1")

strparser :: [Char] -> [Char] -> Parser String
strparser (c:[]) str = char c >> (return str)
strparser (c:cs) str = char c >> (strparser cs str)

strpAux :: [Char] -> Parser Char
strpAux (c:[]) = char c
strpAux (c:cs) = char c >> (strpAux cs)

strp2 :: [Char] -> Parser String
strp2 cs = strpAux cs >> return cs

strp3 :: [Char] -> Parser String
strp3 cs = strpAux cs >> return cs <* eof

testParse :: Show a => Parser a -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n':s)

--main = do
-- pNL "stop:"
-- testParse stop
-- pNL "one:"
-- testParse one
-- pNL "one':"
-- testParse one'
-- pNL "oneTwo:"
-- testParse oneTwo
-- pNL "oneTwo':"
-- testParse oneTwo'
-- pNL "oneTwoThree:"
-- testParse oneTwoThree
 
