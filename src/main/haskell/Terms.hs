module Hee.Terms where

type Id
  = String

-- Terms are either a composition of two functions, or a higher-order
-- function (a string of composed functions)
data Term
  = TmCompose Term Term
  | TmQuote Term
  | TmEmpty
  | TmName Id
  | TmLiteral Literal
  deriving (Eq, Show)

data Literal
  = LiInt Int
  | LiChar Char
  | LiFloat Float
  | LiString String
  deriving (Eq, Show)
