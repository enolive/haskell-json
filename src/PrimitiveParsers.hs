module PrimitiveParsers (stringLiteral, integerOrDouble) where

import qualified Text.Parsec.Token as Token
import Data.Functor.Identity (Identity)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec (try, (<|>), char)

tokenParser :: Token.GenTokenParser String u Identity
tokenParser = Token.makeTokenParser emptyDef

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral tokenParser

integerOrDouble :: Parser (Either Integer Double)
integerOrDouble = (Right <$> try double) <|> (Left <$> integer)

integer :: Parser Integer
integer = Token.integer tokenParser

double :: Parser Double
double = sign <*> Token.float tokenParser
  where
    sign = (negate <$ char '-') <|> (id <$ char '+') <|> pure id

