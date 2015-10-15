import System.Environment
import Control.Monad
import Control.Applicative
import Data.Char

import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Parsec (parse)
import Text.Parsec.String.Char (oneOf, char, digit ,string, letter, satisfy, anyChar)
import Text.Parsec.String.Combinator (many1, anyToken)
import Text.Parsec.Combinator (choice)

main :: IO ()
main = do
    a <- getArgs
    case a of
      [str] -> either print print $ parse myParser "" str
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
