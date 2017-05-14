module Parser where

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
reservedWords = ["not", "while", "let", "true", "false", "if", "then", "else"]

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

bool :: Parser BoolExpr
bool = t <|> f <?> "boolean literal"
  where t = B True <$ reserved "true"
        f = B False <$ reserved "false"

int :: Parser Int
int = fmap fromInteger (lexeme L.integer <?> "integer literal")

langParser :: Parser Stmt
langParser = between spaceConsumer eof stmt

stmt :: Parser Stmt
stmt = parens stmt <|> stmtSequence

stmtSequence :: Parser Stmt
stmtSequence = f <$> sepEndBy stmt' semi
  where f l
          | length l == 1 = head l
          | otherwise = Seq l

stmt' :: Parser Stmt
stmt' = whileStmt
  <|> ifStmt
  <|> letStmt
  <|> arStmt
  <|> blStmt
  <|> (eof >> return NoOp)
  <|> (semi >> return NoOp)

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- bExpr
  -- symbol "{"
  body <- curlies stmt 
  -- symbol "}"
  return (While cond body)

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  c <- bExpr
  reserved "then"
  t <- stmtSequence
  reserved "else"
  e <- stmtSequence
  return (If c t e)

letStmt :: Parser Stmt
letStmt = do
  reserved "let"
  var <- identifier
  void $ symbol "="
  s <- arStmt
  return (Let var s)

arStmt :: Parser Stmt
arStmt = AR <$> aExpr

blStmt :: Parser Stmt
blStmt = BL <$> bExpr

bTerm :: Parser BoolExpr
bTerm = parens bExpr
  <|> (reserved "true" *> pure (B True))
  <|> (reserved "false" *> pure (B False))
  <|> rExpr

rExpr :: Parser BoolExpr
rExpr = do
  one <- aExpr
  op <- relation
  two <- aExpr
  return (RBinary op one two)

relation :: Parser RelBoolOp
relation = pure Equal <* symbol "=="
           <|> pure Less <* symbol "<"
           <|> pure Greater <* symbol ">"
           <|> pure Equal <* symbol "!="

aTerm :: Parser ArExpr
aTerm = parens aExpr
  <|> V <$> identifier
  <|> (I . toInteger) <$> int

bExpr :: Parser BoolExpr
bExpr = makeExprParser bTerm bOperators

aExpr :: Parser ArExpr
aExpr = makeExprParser aTerm aOperators

bOperators :: [[Operator Parser BoolExpr]]
bOperators =
  [ [ Prefix (Not <$ reserved "not") ]
  , [ InfixL (BBinary And <$ symbol "&&")
    , InfixL (BBinary Or <$ symbol "||")
    ]
  ]

aOperators :: [[Operator Parser ArExpr]]
aOperators =
  [ [ Prefix (Neg <$ symbol "~") ]
  , [ InfixL (ABinary Add <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-")
    , InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide <$ symbol "/")
    ]
  ]

