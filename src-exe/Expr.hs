module Expr where

import Common
import Data.Text

data Expr
  = IntLiteral Int
  | StringLiteral Text
  | BoleanLiteral Bool
  | Application (Id, [Expr])
  | Ident Id
  | BinOperation
      { left :: Expr,
        operator :: BinOp,
        right :: Expr
      }
  | BooleanOperation
      { left :: Expr,
        operator' :: BoolOp,
        right :: Expr
      }
  | StructInstance
      { typename :: Maybe Id,
        fields :: [(Id, Expr)]
      }
  | ArrInstance [Expr]
  deriving (Show, Eq)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  deriving (Show, Eq)

data BoolOp
  = Eq
  | Neq
  | Or
  | Xor
  | And
  | LeT
  | GeT
  | Leq
  | Geq
  deriving (Show, Eq)
