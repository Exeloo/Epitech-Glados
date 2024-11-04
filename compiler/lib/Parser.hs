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
    (\key sym value -> [key, sym, value])
    <$> parseSSymbol
    <*> (SSymbol ":" <$ (spaces *> char ':'))
    <*> (spaces *> parseSExpr)

parseSInt :: SexprParser SExpr
parseSInt = SInt . read <$> some digitChar <* notFollowedBy alphaNumChar

parseSFloat :: SexprParser SExpr
parseSFloat = SFloat . read <$> ((++) <$> some digitChar <*> ((:) <$> char '.' <*> some digitChar)) <* notFollowedBy alphaNumChar

parseSBool :: SexprParser SExpr
parseSBool = SBool True <$ string "true" <|> SBool False <$ string "false"

parseSString :: SexprParser SExpr
parseSString = SString <$> (char '"' *> someTill charLiteral (char '"') <|> char '\'' *> someTill charLiteral (char '\''))

parseSSymbol :: SexprParser SExpr
parseSSymbol = SSymbol <$>
    (string "+" <|>
    string "==" <|>
    string "<=" <|>
    string ">=" <|>
    string "&&" <|>
    string "||" <|>
    string "!=" <|>
    string "-" <|>
    string "*" <|>
    string "/" <|>
    string "%" <|>
    string "<" <|>
    string ">" <|>
    string "!" <|>
    string "=" <|>
    some (alphaNumChar <|> oneOf "_"))

parseSArray :: SexprParser SExpr
parseSArray = SArray <$> (char '[' *> spaces *> parseElems <* spaces <* char ']' <* spaces)
    where parseElems = sepBy (spaces *> parseSExpr <* spaces) (char ',')

parseSStruct :: SexprParser SExpr
parseSStruct = SStruct <$> (char '{' *> sepBy (spaces *> parseKeyValue <* spaces) (char ',') <* char '}')

parseSParenthesis :: SexprParser SExpr
parseSParenthesis = SParenthesis <$> (char '(' *> sepBy (spaces *> (SLine <$> sepBy (spaces *> parseSExpr <* spaces) spaces) <* spaces) (char ',') <* char ')')

parseForSParenthesis :: SexprParser SExpr
parseForSParenthesis = SParenthesis <$> (char '(' *> sepBy (spaces *> (SLine <$> sepBy (spaces *> parseSExpr <* spaces) spaces) <* spaces) (char ';') <* char ')')

parseSBracket :: SexprParser SExpr
parseSBracket = SBracket <$> (char '{' *> sepBy (spaces *> parseSLine <* spaces) spaces <* char '}')

parseSExpr :: SexprParser SExpr
parseSExpr = try parseSFloat <|> parseSInt <|> parseSBool <|> parseSString <|> parseSSymbol <|> parseSStruct <|> parseSArray <|> parseSParenthesis

parseSLine :: SexprParser SExpr
parseSLine = SLine <$> (try ((\fnName par bracket -> [SSymbol "function", fnName] ++ par ++ bracket)
    <$> (string "function" *> spaces *> parseSSymbol <* spaces)
    <*> (spaces *> ((: []) <$> parseSParenthesis) <* spaces)
    <*> (spaces *> ((: []) <$> parseSBracket)) <* spaces) <|>
    try ((\sym par bracket -> [sym] ++ par ++ bracket)
    <$> parseSSymbol
    <*> (spaces *> ((: []) <$> parseForSParenthesis) <* spaces)
    <*> (spaces *> ((: []) <$> parseSBracket)) <* spaces) <|>
    sepBy (spaces *> parseSExpr <* spaces) spaces <* char ';')

parseSBlock :: SexprParser SExpr
parseSBlock = SLine <$> sepBy1 (spaces *> parseSLine <* spaces) spaces <* eof
