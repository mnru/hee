module Language.Hee.Simplify
  ( Simplify(..)
  ) where

import Language.Hee.Syntax

class Simplify a where
  simplify :: a -> a

instance Simplify Expression where
  simplify (ECompose (ECompose a b) c)
    = simplify $ ECompose a (ECompose b c)
  simplify (ECompose a b)
    = case (simplify a, simplify b) of
        (EEmpty, b') -> b'
        (a', EEmpty) -> a'
        (a', b')      -> ECompose a' b'
  simplify (EQuote a)
    = EQuote $ simplify a
  simplify (EAnnotate e t)
    = EAnnotate (simplify e) (simplify t)
  simplify e
    = e

instance Simplify Type where
  simplify = id
