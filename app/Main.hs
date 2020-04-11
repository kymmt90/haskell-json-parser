module Main where

import Lib

import Data.Char (digitToInt)
import Text.Parsec
import Text.Parsec.String

data JsonValue = JNumber Int
               deriving (Eq, Show)

parseJson :: Parser JsonValue
parseJson = JNumber <$> number <?> "integer"

number = do
  n <- many1 digit
  return . read $ n

main :: IO ()
main = do
  parseTest parseJson "123"
