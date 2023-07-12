{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Common
import Control.Applicative
import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.Attoparsec.Text qualified as P
import Data.Char (isDigit, isLetter, isSpace)
import Data.Text qualified as T
import Expr
import Stmt

ws :: Parser ()
ws = skipSpace

ws1 :: Parser ()
ws1 = skip isSpace *> ws

intLiteral :: Parser Expr
intLiteral = IntLiteral <$> decimal

stringLiteral :: Parser Expr
stringLiteral = (StringLiteral . T.pack) <$> (char '"' *> manyTill anyChar (char '"'))

boolLiteral :: Parser Expr
boolLiteral =
  "true" *> pure (BoleanLiteral True) <|> "false" *> pure (BoleanLiteral False)

parseLiteral :: Parser Expr
parseLiteral = intLiteral <|> stringLiteral <|> boolLiteral

parseId :: Parser Id
parseId = do
  first <- takeWhile1 isLetter
  second <- P.takeWhile (\c -> isLetter c || isDigit c || c == '_')
  return (Id $ first `T.append` second)

parseIdent :: Parser Expr
parseIdent = Ident <$> parseId

parseApplication :: Parser Expr
parseApplication = do
  id' <- parseId <* ws
  char '(' <* ws
  params <- parseExpr `sepBy` (ws *> char ',' <* ws)
  char ')'
  return $ Application (id', params)

parseBinOperator :: Parser BinOp
parseBinOperator =
  choice
    [ char '+' *> pure Add,
      char '-' *> pure Sub,
      char '*' *> pure Mul,
      char '/' *> pure Div,
      char '%' *> pure Mod
    ]

parseBinOperation :: Parser Expr
parseBinOperation = do
  left <- customExpr <* ws
  operator <- parseBinOperator <* ws
  right <- customExpr
  return $ BinOperation {left, operator, right}
  where
    customExpr = intLiteral <|> parseIdent

parseBoolOperator :: Parser BoolOp
parseBoolOperator =
  choice
    [ "==" *> pure Eq,
      "!=" *> pure Neq,
      "||" *> pure Or,
      "^" *> pure Xor,
      "&&" *> pure And,
      "<=" *> pure Leq,
      ">=" *> pure Geq,
      "<" *> pure LeT,
      ">" *> pure GeT
    ]

-- TODO: implement factor "()"
parseBoolOperation :: Parser Expr
parseBoolOperation = do
  left <- customExpr <* ws
  operator' <- parseBoolOperator <* ws
  right <- customExpr
  return $ BooleanOperation {left, operator', right}
  where
    customExpr =
      choice
        [ parseBinOperation,
          parseLiteral,
          parseStructInstance,
          parseApplication,
          -- parseBoolOperation, FIXME: infinite loop
          parseIdent
        ]

parseStructInstance :: Parser Expr
parseStructInstance = do
  typename <- Just <$> parseId <* ws <|> (\_ -> Nothing) <$> char '.'
  char '{' *> ws
  fields <- manyTill field' (char '}') <* ws
  return $ StructInstance {typename, fields}
  where
    field' = do
      id' <- char '.' *> parseId <* ws
      char '=' *> ws
      expr <- parseExpr <* ws
      char ';' *> ws
      return ((id', expr))

parseArrInstance :: Parser Expr
parseArrInstance = do
  char '[' *> ws
  values <- parseExpr `sepBy` (ws <* char ',' <* ws) <* ws
  char ']' *> ws
  return $ ArrInstance values

parseExpr :: Parser Expr
parseExpr =
  choice
    [ parseBinOperation,
      parseBoolOperation,
      parseLiteral,
      parseStructInstance,
      parseArrInstance,
      parseApplication,
      parseIdent
    ]

parseStmt :: Parser Stmt
parseStmt =
  choice
    [ parseIf,
      parseFor,
      parseWhile,
      parseFuncDef,
      parseStructDef,
      parseReturn,
      Expression <$> parseExpr <* ws <* char ';',
      parseVar,
      Block <$> parseBlock
    ]
    <* ws

parseVar :: Parser Stmt
parseVar = do
  typeId <- parseId
  arr <- ("[]" <|> ws *> pure "") <* ws1
  let tid = case typeId of
        Id (x) -> Id (x `T.append` arr)

  ident <- parseId <* ws
  char '=' *> ws
  val <- parseExpr <* ws <* char ';'
  return $ Var {typeId = tid, ident, val}

parseBlock :: Parser Block
parseBlock = do
  char '{' *> ws
  stmts <- manyTill (parseStmt <* ws) (char '}' *> ws)
  return stmts

parseIf :: Parser Stmt
parseIf = do
  "if" *> ws1
  cond <- parseBoolOperation <* ws1
  if_block <- parseBlock <* ws
  else_block <-
    Just <$> ("else" *> ws *> parseBlock <* ws) <|> (\_ -> Nothing) <$> ws
  return $ If {cond, body = if_block, else' = else_block}

parseFor :: Parser Stmt
parseFor = do
  "for" *> ws1
  ident <- parseId <* ws1
  "in" *> ws1
  iterator <- parseId <* ws
  body <- parseBlock <* ws
  return $ ForLoop {forRange = (ident, iterator), body}

parseWhile :: Parser Stmt
parseWhile = do
  "while" *> ws
  cond <- (boolLiteral <|> parseIdent <|> parseBoolOperation) <* ws
  body <- parseBlock
  return $ While {cond, body}

parseFuncDef :: Parser Stmt
parseFuncDef = do
  "fun" *> ws
  name <- parseId <* ws
  char '(' *> ws
  fields' <- field `sepBy` (ws <* char ',' <* ws)
  char ')' *> ws *> char ':' <* ws
  typeId <- parseId <* ws
  body <- parseBlock
  return $ FuncDef {name, fields', typeId, body}
  where
    field = do
      typ <- parseId <* ws1
      nam <- parseId
      return ((typ, nam))

parseStructDef :: Parser Stmt
parseStructDef = do
  name <- "struct" *> ws1 *> parseId <* ws
  char '{' *> ws
  fields' <- manyTill field (char '}' <* ws)
  return $ StructDef {name, fields'}
  where
    field = do
      typ <- parseId <* ws1
      nam <- parseId <* ws <* char ';' <* ws
      return ((typ, nam))

parseReturn :: Parser Stmt
parseReturn = Return <$> ("return" *> ws *> parseExpr <* ws <* char ';')

parseTopLevel :: Parser Stmt
parseTopLevel = parseStructDef <|> parseFuncDef

runParser :: T.Text -> Either String [Stmt]
runParser = parseOnly (many parseTopLevel)

run :: T.Text -> Parser a -> Either String a
run = flip parseOnly
