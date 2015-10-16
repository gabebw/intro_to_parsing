import System.Environment
import Control.Monad
import Control.Applicative
import Data.Char

import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Parsec (parse)
import Text.Parsec.String.Char (oneOf, char, digit ,string, letter, satisfy, anyChar)
import Text.Parsec.String.Combinator (many1, anyToken, eof, manyTill, lookAhead)
import Text.Parsec.Combinator (choice)
import Text.ParserCombinators.Parsec (try)

data Chunk = Chunk { left :: String, right :: String }
    deriving Show

main :: IO ()
main = do
    a <- getArgs
    case a of
      [str] -> either print print $ parse (parseAround "heart") "" str
      _ -> error "please pass one argument with the string to parse"

myParser :: Parser Integer
myParser = num

num :: Parser Integer
num = do
    n <- lexeme $ many1 digit
    return (read n)

-- Parse `p` followed by whitespace, and discard the whitespace
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- Parses (and then ignores) any whitespace
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

rhymes :: Parser String
rhymes = choice [string "art", string "cart"]

-- Parse before and after a word
parseAround word = do
    before <- manyTill anyToken (lookAhead $ try $ (void $ string word))
    string word
    after <- many anyToken
    return $ Chunk before after

-- runhaskell ParseString.hs "horse before the heart right"
-- Try on "hey there heart hey"
ugh :: Parser String
ugh = do
    manyTill (anyChar) (try $ string "heart")
    whitespace
    x <- string "heart"
    whitespace
    return x
    try (many anyChar)
