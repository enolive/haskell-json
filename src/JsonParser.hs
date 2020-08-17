module JsonParser (jsonValue, JsonValue(..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import PrimitiveParsers
import Text.Parsec
import Text.Parsec.String (Parser)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonDouble Double
  | JsonInteger Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject (Map String JsonValue)
  deriving (Show, Eq)

jsonValue :: Parser JsonValue
jsonValue =
  jsonNull
    <|> jsonBool
    <|> jsonNumber
    <|> jsonString
    <|> jsonArray
    <|> jsonObject

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ string "null"

jsonBool :: Parser JsonValue
jsonBool = JsonBool <$> (True <$ string "true" <|> False <$ string "false")

jsonNumber :: Parser JsonValue
jsonNumber = f <$> integerOrDouble
  where
    f (Left int') = JsonInteger int'
    f (Right double') = JsonDouble double'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (char '[' *> spaces *> elements <* spaces <* char ']')
  where
    -- try lookahead important as this collides with the upper definition of leading spaces
    elements = sepBy jsonValue (try (spaces *> char ',' *> spaces))

jsonObject :: Parser JsonValue
jsonObject = JsonObject . Map.fromList <$> (char '{' *> spaces *> pairs <* spaces <* char '}')
  where
    -- try lookahead important as this collides with the upper definition of leading spaces
    pairs = sepBy pair (try (spaces *> char ',' *> spaces))
    pair = (,) <$> (stringLiteral <* spaces <* char ':' <* spaces) <*> jsonValue
