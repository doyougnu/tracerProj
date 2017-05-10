module Parser where

import Data.Text (Text, pack)
import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

import Lang

-- | Lexer

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineCmnt blkCmnt
  where lineCmnt = L.skipLineComment "!!"
        blkCmnt  = L.skipBlockComment "!*" "*!"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (lexeme $ symbol "(") (lexeme $ symbol ")")

curlies :: Parser a -> Parser a
curlies = between (lexeme $ symbol "{") (lexeme $ symbol "}")

semi :: Parser String
semi = symbol ";"

reserved :: String -> Parser ()
reserved str = lexeme $ string str >> notFollowedBy alphaNumChar

reservedWords :: [String]
reservedWords = ["not", "while", "let", "true", "false"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x
      | x `elem` reservedWords = fail $ "keyword " ++ show x ++ " is reserved"
      | otherwise = return x

-- | Statement Parser

unit :: Parser ()
unit = () <$ symbol "()" <?> "unit literal"

bool :: Parser Bool
bool = t <|> f <?> "boolean literal"
  where t = True <$ reserved "true"
        f = False <$ reserved "false"

int :: Parser Int
int = fmap fromInteger (lexeme L.integer <?> "integer literal")

whileParser :: Parser Stmt
whileParser = between spaceConsumer eof stmt

stmt :: Parser Stmt
stmt = parens stmt <|> stmtSequence

stmtSequence :: Parser Stmt
stmtSequence = f <$> sepBy1 stmt' semi
  where f l
          | length l == 1 = head l
          | otherwise = Seq l

stmt' :: Parser Stmt
stmt' = whileStmt <|> noopStmt <|> letStmt

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- bExpr
  body <- curlies stmt
  return (While cond body)

bterm :: Parser Stmt
bterm = parens bExpr
  <|> (reserved "true" *> pure (L $ LBool True)) 
  <|> (reserved "false" *> pure (L $ LBool False)) 

bExpr :: Parser Stmt
bExpr = makeExprParser bterm bOperators

bOperators :: 
