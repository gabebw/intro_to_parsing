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

-- runhaskell ParseString.hs "horse before the heart right"
main :: IO ()
main = do
    a <- getArgs
    case a of
      [w, str] -> either print (print . reconstruct w) $ parse myParser "" str
      _ -> error "please pass one argument with the string to parse"

myParser :: Parser Chunk
myParser = choice $ map (try . parseAround) ["heart", "cart"]

-- Parse before/after a word into a Chunk
parseAround :: String -> Parser Chunk
parseAround word = do
    before <- manyTill anyToken (lookAhead $ try $ (void $ string word))
    string word
    after <- many anyToken
    return $ Chunk before after

reconstruct :: String -> Chunk -> String
reconstruct word (Chunk before after) = before ++ word ++ after
