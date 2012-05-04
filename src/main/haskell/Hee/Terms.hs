module Hee.Terms
  ( Term(..)
  , Literal(..)
  , showTerm
  , showLiteral
  , simplify
  ) where

import Data.ByteString       as B
import Data.ByteString.Char8 as C
import Data.Word
import Hee.Types (Type)

type Id
  = String

-- By construction, every term is a function
-- * TmCompose is a composition of two functions
-- * TmLiteral is a nullary function which returns one value
-- * TmQuote is a function literal
-- * TmEmpty is the identity function
-- * TmName is a function identified by name
data Term
  = TmCompose Term Term
  | TmQuote Term
  | TmEmpty
  | TmName Id
  | TmLiteral Literal
  | TmAnnotate Type Term
  deriving (Eq, Show)

data Literal
  = LInt Int
  | LChar Word8
  | LRatn Double
  | LString ByteString
  deriving (Eq, Show)

showTerm :: Term -> String
showTerm TmEmpty                = ""
showTerm (TmName id)            = id
showTerm (TmQuote t)            = "[" ++ showTerm t ++ "]"
showTerm (TmLiteral t)          = showLiteral t
showTerm (TmCompose TmEmpty t)  = showTerm t
showTerm (TmCompose s t)        = showTerm s ++ " " ++ showTerm t

showLiteral :: Literal -> String
showLiteral (LInt l)    = show l
showLiteral (LChar l)   = show l
showLiteral (LRatn l)   = show l
showLiteral (LString l) = show l

--instance Show Term where
--  show = showTerm

--instance Show Literal where
--  show = showLiteral

simplify :: Term -> Term
simplify (TmCompose TmEmpty x) = simplify x
simplify (TmCompose x TmEmpty) = simplify x
simplify (TmCompose x y)       = TmCompose (simplify x) (simplify y)
simplify (TmQuote x)           = TmQuote (simplify x)
simplify x                     = x
