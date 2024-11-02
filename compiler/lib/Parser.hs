{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- Parser
-}


module Parser(parseSExpr, parseSString, parseSSymbol, parseSInt, parseSBracket, parseSLine, parseSBlock) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void
import SExprData

type SexprParser = Parsec Void String

spaces :: SexprParser ()
spaces = skipMany spaceChar

parseKeyValue :: SexprParser [SExpr]
parseKeyValue =
    (\key value -> [key, value])
    <$> parseSSymbol
    <*> (spaces *> char ':' *> spaces *> parseSExpr)

-- TO DO TOO
parseSInt :: SexprParser SExpr
parseSInt = SInt . read <$> some digitChar

-- TO DO
parseSFloat :: SexprParser SExpr
parseSFloat = SFloat <$> float

parseSBool :: SexprParser SExpr
parseSBool = (SBool True <$ string "true") <|> (SBool False <$ string "false")

parseSString :: SexprParser SExpr
parseSString = SString <$> ((char '"' *> someTill charLiteral (char '"')) <|> (char '\'' *> someTill charLiteral (char '\'')))

parseSSymbol :: SexprParser SExpr
parseSSymbol = SSymbol <$> some (alphaNumChar <|> oneOf "+-*<?=")

parseSArray :: SexprParser SExpr
parseSArray = SArray <$> (char '[' *> spaces *> parseElems <* spaces <* char ']')
    where parseElems = sepBy parseSExpr (spaces *> char ',' <* spaces)

parseSStruct :: SexprParser SExpr
parseSStruct = SStruct <$> (spaces *> char '{' *> spaces *> sepBy parseKeyValue (spaces *> char ',' *> spaces) <* spaces <* char '}' <* char ';' <* spaces)

parseSParenthesis :: SexprParser SExpr
parseSParenthesis = SParenthesis <$> (spaces *> char '(' *> (spaces *> (sepBy parseSExpr (spaces *> char ',' <* spaces)) <* spaces) <* char ')' <* spaces)

parseSBracket :: SexprParser SExpr
parseSBracket = SBracket <$> (spaces *> char '{' *> some (spaces *> parseSExpr <* spaces) <* char '}' <* spaces)

parseSLine :: SexprParser SExpr
parseSLine = SLine <$> ((some (spaces *> parseSExpr <* spaces)) <|> (spaces *> (some (spaces *> parseSExpr <* spaces)) <* char ';' <* spaces))
-- parseSLine = SLine <$> ((spaces *> some (spaces *> parseSExpr <* spaces) *> char '{' *> some (spaces *> parseSExpr <* spaces) <* char '}' <* spaces) <|> (spaces *> (spaces *> parseSExpr <* spaces) *> char '{' *> (spaces *> parseSExpr <* spaces) <* char '}' <* spaces) <|> (spaces *> (sepBy parseSExpr (spaces *> char ' ' <* spaces)) <* char ';' <* spaces))

parseSExpr :: SexprParser SExpr
parseSExpr = parseSInt <|> parseSSymbol

parseSBlock :: SexprParser SExpr
parseSBlock = SLine <$> some parseSLine <* eof
