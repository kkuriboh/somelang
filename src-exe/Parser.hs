-- TODO: refactor everything
{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}

module Parser where

import Common (Id(Id))
import Control.Applicative
import Data.Attoparsec.Text hiding (take)
import Data.Char (isSpace, ord)
import qualified Data.Text as T
import Expr
import Stmt

ws :: Parser ()
ws = skipSpace

intLiteral :: Parser Expr
intLiteral = IntLiteral <$> decimal

stringLiteral :: Parser Expr
stringLiteral =
  (StringLiteral . T.pack) <$> (char '"' *> many anyChar <* char '"')

boolLiteral :: Parser Expr
boolLiteral =
  "true" *> pure (BoleanLiteral True) <|> "false" *> pure (BoleanLiteral False)

parseLiteral :: Parser Expr
parseLiteral = intLiteral <|> stringLiteral <|> boolLiteral

parseId :: Parser Id
parseId = do
  first <- takeWhile1 (nonDigit . ord)
  second <- takeTill isSpace <* ws
  return (Id $ first `T.append` second)
  where
    nonDigit c
      | (c >= 65 && c <= 90) || (c >= 97 && c <= 122) = True
      | otherwise = False

parseApplication :: Parser Expr
parseApplication = do
  id' <- parseId
  "(" *> ws
  params <- many parseExpr <* ws <* char ','
  ")" *> ws
  return (Application (id', params))

parseBinOperator :: Parser BinOp
parseBinOperator = operator <$> anyChar
  where
    operator '+' = Add
    operator '-' = Sub
    operator '*' = Mul
    operator '/' = Div
    operator '%' = Mod
    operator _ = error "invalid operator"

parseBinOperation :: Parser Expr
parseBinOperation = do
  left <- customExpr <* ws
  operator <- parseBinOperator <* ws
  right <- customExpr <* ws
  return (BinOperation {left, operator, right})
  where
    customExpr = choice [IntLiteral <$> decimal, Ident <$> parseId]

parseBoolOperator :: Parser BoolOp
parseBoolOperator = operator <$> many anyChar
  where
    operator "=" = Eq
    operator "!=" = Neq
    operator "||" = Or
    operator "^" = Xor
    operator "&&" = And
    operator "<" = LeT
    operator ">" = GeT
    operator "<=" = Leq
    operator ">=" = Geq
    operator _ = error "invalid operator"

parseBoolOperation :: Parser Expr
parseBoolOperation = do
  left <- customExpr <* ws
  operator' <- parseBoolOperator <* ws
  right <- customExpr <* ws
  return (BooleanOperation {left, operator', right})
  where
    customExpr = choice [boolLiteral, Ident <$> parseId, parseBoolOperation]

parseStructInstance :: Parser Expr
parseStructInstance = do
  typename <- Just <$> parseId <* ws <|> (\_ -> Nothing) <$> char '.'
  "{" *> ws
  fields <- many field
  "}" *> ws
  return (StructInstance {typename, fields})
  where
    field = do
      id' <- char '.' *> parseId <* ws <* char '=' <* ws
      expr <- parseExpr <* ws <* char ';' <* ws
      return ((id', expr))

parseExpr :: Parser Expr
parseExpr =
  choice
    [ Ident <$> parseId
    , parseLiteral
    , parseApplication
    , parseBinOperation
    , parseBoolOperation
    , parseStructInstance
    ]

parseStmt :: Parser Stmt
parseStmt =
  choice
    [ parseIf
    , Expression <$> parseExpr
    , Block <$> parseBlock
    , parseVar
    , parseFor
    , parseWhile
    , parseFuncDef
    , parseStructDef
    ]

parseVar :: Parser Stmt
parseVar = do
  typeId <- parseId <* ws
  ident <- parseId <* ws
  val <- parseExpr <* ws <* char ';'
  return (Var {typeId, ident, val})

parseBlock :: Parser Block
parseBlock = do
  "{" *> ws
  stmts <- many parseStmt <* ws
  "}" *> ws
  return stmts

parseIf :: Parser Stmt
parseIf = do
  "if" *> ws
  cond <- parseExpr <* ws
  if_block <- parseBlock <* ws
  "else" *> ws
  else_block <- parseBlock <* ws
  return (If {cond, body = if_block, else' = Just else_block})

parseFor :: Parser Stmt
parseFor = do
  "for" *> ws
  ident <- parseId <* ws
  "in" *> ws
  iterator <- parseId <* ws
  body <- parseBlock <* ws
  return (ForLoop {forRange = (ident, iterator), body})

parseWhile :: Parser Stmt
parseWhile = do
  "while" *> ws
  cond <- (boolLiteral <|> Ident <$> parseId) <* ws
  body <- parseBlock
  return (While {cond, body})

parseFuncDef :: Parser Stmt
parseFuncDef = do
  "fun" *> ws
  name <- parseId <* ws
  char '(' *> ws
  params <- many field
  char ')' *> ws *> char ':'
  typ <- parseId <* ws
  body <- parseBlock
  return (FuncDef (name, params, typ, body))

  where
    field = do
      typ <- parseId <* ws
      nam <- parseId <* ws <* char ','
      return ((typ, nam))

parseStructDef :: Parser Stmt
parseStructDef = do
  "struct" *> ws
  name <- parseId <* ws
  char '{' *> ws
  fields' <- many field
  char '}' *> ws
  return (StructDef {name, fields'})

  where
    field = do
      typ <- parseId <* ws
      nam <- parseId <* ws <* char ';'
      return ((typ, nam))

parseReturn :: Parser Stmt
parseReturn = Return <$> ("return" *> ws *> parseExpr <* ws <* char ';')

runParser :: T.Text -> Either String Stmt
runParser t = parseOnly parseStmt t
