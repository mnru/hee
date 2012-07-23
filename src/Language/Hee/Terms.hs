module Language.Hee.Terms
  ( Term(..)
  , Literal(..)
  , simplify
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
  | TmComment Text          --
  deriving (Eq, Show)

data Literal
  = LiChar Char
  | LiString Text
  | LiNumber Int
  deriving (Eq, Show)

simplify :: Term -> Term
simplify (TmQuote a)
  = case simplify a of
      TmEmpty -> TmEmpty
      a'      -> TmQuote a'
simplify (TmCompose a b)
  = case (simplify a, simplify b) of
      (TmEmpty, b') -> b'
      (a', TmEmpty) -> a'
      (a', b')      -> TmCompose a' b'
simplify (TmAnnotation e t)
  = TmAnnotation (simplify e) t
simplify e
  = e
