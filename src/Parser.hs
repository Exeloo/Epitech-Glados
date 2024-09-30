module Parser(parseSExpr, parseSString, parseSSymbol, parseSInt, parseSList) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void
import SExprData

type SexprParser = Parsec Void String

spaces :: SexprParser ()
spaces = skipMany spaceChar

parseSString :: SexprParser SExpr
parseSString = SString <$> ((char '"' *> someTill charLiteral (char '"')) <|> (char '\'' *> someTill charLiteral (char '\'')))

parseSSymbol :: SexprParser SExpr
parseSSymbol = SSymbol <$> some (alphaNumChar <|> oneOf "-_+<>?")

parseSInt :: SexprParser SExpr
parseSInt = SInt . read <$> some digitChar

parseSList :: SexprParser SExpr
parseSList = SList <$> (spaces *> char '(' *> some (spaces *> parseSExpr <* spaces) <* char ')' <* spaces)

parseSExpr :: SexprParser SExpr
parseSExpr = parseSInt <|> parseSList <|> parseSString <|> parseSSymbol
