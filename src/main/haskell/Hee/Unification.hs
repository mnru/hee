module Hee.Unification
  ( CanUnify(..)
  ) where

import Data.List (nub, intersect, union)
import Hee.Substitution
import Hee.Kinds
import Hee.Types

class CanUnify t where
  match   :: Monad m => t -> t -> m Substitution
  unify   :: Monad m => t -> t -> m Substitution
  bindvar :: Monad m => Variable -> t -> m Substitution

instance CanUnify Type where
  match (TStack s) (TStack s') = match s s'
  match (TVariable id k) t     = bindvar (id,k) t
  match (TConstructor id k) (TConstructor id' k')
    | id == id' && k == k'      = return empty
  match (TApplication i o) (TApplication i' o')
                                = do a <- match i i'
                                     b <- match (substitute a o) (substitute a o')
                                     merge a b
  match _ _                     = fail "merge failed"

  unify (TStack s) (TStack s') = unify s s'
  unify (TVariable id k) t     = bindvar (id,k) t
  unify t (TVariable id k)     = bindvar (id,k) t
  unify (TConstructor id k) (TConstructor id' k')
    | id == id' && k == k'     = return empty
  unify (TApplication i o) (TApplication i' o')
                               = do a <- unify i i'
                                    b <- unify (substitute a o) (substitute a o')
                                    return (b @@ a)
  unify _ _                    = fail "unify failed"

  bindvar v@(id,k) t
    | t == TVariable id k = return empty
    | v `elem` freeVars t = fail "bindvar failed (occurs check)"
    | k /= kind t         = fail "bindvar failed (kind mismatch)"
    | otherwise           = return (v +-> t)

instance CanUnify Stack where
  match (SBottom id) t = bindvar (id,KStack) t
  match SEmpty SEmpty  = return empty
  match (SPush t h) (SPush t' h')
                       = do a <- match t t'
                            b <- match (substitute a h) (substitute a h')
                            merge a b

  unify (SBottom id) t = bindvar (id,KStack) t
  unify t (SBottom id) = bindvar (id,KStack) t
  unify SEmpty SEmpty  = return empty
  unify (SPush t h) (SPush t' h')
                       = do a <- unify h h'
                            b <- unify (substitute a t) (substitute a t')
                            return (b @@ a)
  unify _ _            = fail "unify failed"

  bindvar v@(id,KStack) t = return (v +-> TStack t)
  bindvar _ _             = fail "bindvar failed (kind mismatch)"
