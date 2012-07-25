module Language.Hee.Simplify
  ( Simplify(..)
  ) where

import Language.Hee.Syntax

class Simplify a where
  simplify :: a -> a

instance Simplify Expr where
  simplify (ExCompose (ExCompose a b) c)
    = simplify $ ExCompose a (ExCompose b c)
  simplify (ExCompose a b)
    = case (simplify a, simplify b) of
        (ExEmpty, b') -> b'
        (a', ExEmpty) -> a'
        (a', b')      -> ExCompose a' b'
  simplify (ExQuote a)
    = ExQuote $ simplify a
  simplify (ExAnnotate e t)
    = ExAnnotate (simplify e) (simplify t)
  simplify e
    = e

instance Simplify Type where
  simplify = id
