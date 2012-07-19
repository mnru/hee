module Language.Hee.Terms
  ( Term(..)
  ) where

import Language.Hee.Types (Type)
import Data.Text

data Term
  = TmEmpty                 --
  | TmName Text             --
  | TmQuote Term            --
  | TmLiteral Literal       --
  | TmCompose Term Term     --
  | TmAnnotation Term Type  --
  deriving (Eq, Show)

data Literal
  = LiChar
  | LiString
  | LiNumber
  deriving (Eq, Show)

