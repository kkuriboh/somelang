module Stmt where

import Common
import Expr

data Stmt
  = Var
      { typeId :: Id,
        ident :: Id,
        val :: Expr
      }
  | Expression Expr
  | Block Block
  | If
      { cond :: Expr,
        body :: Block,
        else' :: Maybe Block
      }
  | ForLoop
      { forRange :: (Id, Id),
        body :: Block
      }
  | While
      { cond :: Expr,
        body :: Block
      }
  | FuncDef
      { name :: Id,
        fields' :: [(Id, Id)],
        typeId :: Id,
        body :: Block
      }
  | StructDef
      { name :: Id,
        fields' :: [(Id, Id)]
      }
  | Return Expr
  deriving (Show, Eq)

type Block = [Stmt]
