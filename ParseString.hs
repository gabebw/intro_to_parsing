import System.Environment
import Text.Parsec
import Text.Parsec.String
import Control.Monad

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
