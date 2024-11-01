module Parser (parseSExpr) where

import Data.Void
import Data.Char
import Data.Functor
import SExprData (SSysCall(..), SValue(..), SInst(..), SAsm(..), SExpr)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type SExprParser = Parsec Void String

parseJsonArray :: SExprParser a -> SExprParser [a]
parseJsonArray p = char '[' *> many (spacesOrNewLine *>
    p <* spacesOrNewLine <* skipMany (char ',') <* spacesOrNewLine)
    <* spacesOrNewLine <* char ']'

parseCaseString :: String -> SExprParser String
parseCaseString s = try $ traverse (\c -> char (toLower c) <|> char (toUpper c)) s

spacesOrNewLine :: SExprParser ()
spacesOrNewLine = skipMany spaceChar

spaces :: SExprParser ()
spaces = skipSome hspace1

parseStringAlphaNum :: SExprParser String
parseStringAlphaNum = some (alphaNumChar <|> oneOf "+-_")

parseSLabel :: SExprParser SAsm
parseSLabel = SLabel <$> (char '.' *> parseStringAlphaNum <* char ':')

parseSInt :: SExprParser SValue
parseSInt = SInt . read <$> some digitChar

parseSDouble :: SExprParser SValue
parseSDouble = SDouble . read <$> ((++) <$> some digitChar <*> ((:) <$> char '.' <*> some digitChar))

parseSBool :: SExprParser SValue
parseSBool = SBool <$> ((parseCaseString "true" $> True) <|> (parseCaseString "false" $> False))

parseSString :: SExprParser SValue
parseSString = SString <$> ((char '"' *> someTill charLiteral (char '"')) <|> (char '\'' *> someTill charLiteral (char '\'')))

parseSArray :: SExprParser SValue
parseSArray = SArray <$> parseJsonArray parseSValue

parseSValueCall :: SExprParser SValue
parseSValueCall = SValueCall <$> ((parseCaseString "Add" $> SAdd) <|>
    (parseCaseString "Sub" $> SSub) <|>
    (parseCaseString "Mul" $> SMul) <|>
    (parseCaseString "Div" $> SDiv) <|>
    (parseCaseString "Eq" $> SEq) <|>
    (parseCaseString "Less" $> SLess) <|>
    (parseCaseString "Not" $> SNot) <|>
    (parseCaseString "Or" $> SOr) <|>
    (parseCaseString "And" $> SAnd) <|>
    (parseCaseString "AccessArray" $> SAccessArray) <|>
    (parseCaseString "ModifyArray" $> SModifyArray) <|>
    (parseCaseString "AccessObject" $> SAccessObject) <|>
    (parseCaseString "ModifyObject" $> SModifyObject) <|>
    (parseCaseString "Print" $> SPrint))

parseSValue :: SExprParser SValue
parseSValue = parseSInt <|> parseSBool <|> parseSDouble <|> parseSString <|> parseSArray <|> parseSValueCall <|> (parseCaseString "undefined" $> SUndefined)

parseInstruction :: SExprParser SAsm
parseInstruction = (SInstruction <$> ((SPushStackOnArg <$ parseCaseString "PushStackOnArg") <|>
    (SCall <$ parseCaseString "Call" ) <|>
    (SRet <$ parseCaseString "Ret") <|>
    (SJumpIfFalse <$> (parseCaseString "JumpIfFalse" *> spaces *> parseStringAlphaNum)) <|>
    (SPushArgOnStack <$> (parseCaseString "PushArgOnStack" *> spaces *> (read <$> some digitChar))) <|>
    (SPopArg <$ parseCaseString "PopArg") <|>
    (SPopStack <$ parseCaseString "PopStack") <|>
    (SJump <$> (parseCaseString "Jump" *> spaces *> parseStringAlphaNum)) <|>
    (SPushOnStack <$> (parseCaseString "Push" *> spaces *> parseSValue)))) <|>
    parseSLabel

parseSExpr :: SExprParser SExpr
parseSExpr = some (spacesOrNewLine *> parseInstruction <* spacesOrNewLine) <* eof
