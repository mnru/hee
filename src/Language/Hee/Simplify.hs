module Language.Hee.Simplify
  ( Simplify(..)
  ) where

import Language.Hee.Types
import Language.Hee.Terms

class Simplify a where
  simplify :: a -> a

instance Simplify Term where
  simplify (TmCompose a b)
    = case (simplify a, simplify b) of
        (TmEmpty, b') -> b'
        (a', TmEmpty) -> a'
        (a', b')      -> TmCompose a' b'
  simplify (TmQuote a)
    = TmQuote $ simplify a
  simplify (TmAnnotation e t)
    = TmAnnotation (simplify e) t
  simplify e
    = e

instance Simplify Type where
  simplify = id
