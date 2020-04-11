module Main where

import Lib

import Data.Char (digitToInt)
import Text.Parsec
import Text.Parsec.String

data JsonValue = JNumber Int
               | JString String
               deriving (Eq, Show)

parseJson :: Parser JsonValue
parseJson = JNumber <$> (number <?> "integer")
            <|> JString <$> str

number = do
  n <- many1 digit
  return . read $ n

str = char '"' *> many (noneOf ['"']) <* char '"'

main :: IO ()
main = do
  parseTest parseJson "123"
  parseTest parseJson "\"abc\""
