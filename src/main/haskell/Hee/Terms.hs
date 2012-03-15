module Hee.Terms
  ( Term(..)
  , Literal(..)
  ) where

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

showTerm :: Term -> String
showTerm TmEmpty                = ""
showTerm (TmName id)            = id
showTerm (TmQuote t)            = "[" ++ showTerm t ++ "]"
showTerm (TmLiteral t)          = showLiteral t
showTerm (TmCompose TmEmpty t)  = showTerm t
showTerm (TmCompose s t)        = showTerm s ++ " " ++ showTerm t

showLiteral :: Literal -> String
showLiteral (LiInt l)    = show l
showLiteral (LiChar l)   = show l
showLiteral (LiFloat l)  = show l
showLiteral (LiString l) = show l

--instance Show Term where
--  show = showTerm

--instance Show Literal where
--  show = showLiteral
