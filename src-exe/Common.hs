module Common where

import Data.Text

newtype Id
  = Id Text
  deriving (Show, Eq)
