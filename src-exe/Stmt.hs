module Stmt where

import Common
import Expr

data Stmt
  = Var
      { typeId :: Id
      , ident :: Id
      , val :: Expr
      }
  | Expression Expr
  | Block Block
  | If
      { cond :: Expr
      , body :: Block
      , else' :: Maybe Block
      }
  | ForLoop
      { forRange :: (Id, Id)
      , body :: Block
      }
  | While
      { cond :: Expr
      , body :: Block
      }
  | FuncDef (Id, [(Id, Id)], Id, Block)
  | StructDef { name :: Id, fields' :: [(Id, Id)] }
  | Return Expr

type Block = [Stmt]
