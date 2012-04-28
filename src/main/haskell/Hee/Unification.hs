module Hee.Unification
  ( CanUnify(..)
  ) where

import Data.List (nub, intersect, union)
import Hee.Substitution

class CanUnify t where
  match   :: Monad m => t -> t -> m (Substitution t)
  unify   :: Monad m => t -> t -> m (Substitution t)
  bindvar :: Monad m => Variable -> t -> m (Substitution t)
