module Main where

import Lib

import Data.Char (digitToInt)
import Text.Parsec
import Text.Parsec.String

data JsonValue = JNumber Int
               | JString String
               | JBool Bool
               | JNull
               | JArray [JsonValue]
               deriving (Eq, Show)

parseJson :: Parser JsonValue
parseJson = JNumber <$> (number <?> "integer")
            <|> JString <$> str
            <|> JBool True <$ boolTrue
            <|> JBool False <$ boolFalse
            <|> JNull <$ nullValue
            <|> JArray <$> array

number = do
  n <- many1 digit
  return . read $ n

str = char '"' *> many (noneOf ['"']) <* char '"'

boolTrue = string "true"

boolFalse = string "false"

nullValue = string "null"

array = char '[' *> parseJson `sepEndBy` (char ',') <* char ']'

main :: IO ()
main = do
  parseTest parseJson "123"
  parseTest parseJson "\"abc\""
  parseTest parseJson "true"
  parseTest parseJson "false"
  parseTest parseJson "null"
  parseTest parseJson "[1,2,3]"
  parseTest parseJson "[123,null,true,false,\"abc\"]"
