module Main where

import Lib

import Text.Parsec

data JsonValue = JNumber Double
               | JString String
               | JBool Bool
               | JNull
               | JArray [JsonValue]
               | JObject [(String, JsonValue)]
               deriving (Eq, Show)

parseJson = JNumber <$> number
            <|> JString <$> str
            <|> JBool True <$ boolTrue
            <|> JBool False <$ boolFalse
            <|> JNull <$ nullValue
            <|> JArray <$> array
            <|> JObject <$> object

number = do
  integer <- many1 digit
  decimal <- option "" $ (:) <$> char '.' <*> many1 digit
  return . read $ (integer ++ decimal)

str = between (char '"') (char '"' >> spaces) (many $ noneOf ['"'])
boolTrue = string "true" <* spaces
boolFalse = string "false" <* spaces
nullValue = string "null" <* spaces

array = between (char '[' >> spaces) (char ']') ((spaces *> parseJson <* spaces) `sepEndBy` (char ','))

object = do
  char '{'
  spaces
  kvs <- (spaces *> kv <* spaces) `sepEndBy` (char ',')
  spaces
  char '}'
  return kvs

kv = do
  k <- str
  spaces
  char ':'
  spaces
  v <- parseJson
  return (k, v)

main :: IO ()
main = do
  parseTest parseJson "123"
  parseTest parseJson "123.456"
  parseTest parseJson "\"abc\""
  parseTest parseJson "true"
  parseTest parseJson "false"
  parseTest parseJson "null"
  parseTest parseJson "[ 1 , 2 , 3 ]"
  parseTest parseJson "[ 123 , null , true , false , \"abc\" ]"
  parseTest parseJson "{ \"key\" : \"value\" , \"key2\" : 123, \"key3\" : false, \"key4\" : null , \"key5\" : [ 1 , 2 , 3 ] , \"key6\" : { \"nested\" : null } }"
