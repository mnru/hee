module Hee.Stack
  ( Stack(..)
  , showStack
  ) where

import Hee.Kinds
import Hee.Unification
import Hee.Substitution

-- Stacks have the kind KStack and are distinguished from Type because
-- they cannot be used to describe first-class values.
data Stack
  = SEmpty
  | SBottom Id
  | SPush Stack Type
  deriving (Eq, Show)

instance HasKind Stack where
  kind t = KStack

instance CanUnify Stack where
  match (SBottom id) t = bindvar (id,KStack) t
  match SEmpty SEmpty = return empty
  match (SPush t s) (SPush t' s')
                        = do a <- match t t'
                             b <- match (substitute a s) (substitute a s')
                             merge a b

  unify (SBottom id) t = bindvar (id,KStack) t
  unify t (SBottom id) = bindvar (id,KStack) t
  unify SEmpty SEmpty  = return empty
  unify (SPush t s) (SPush t' s')
                       = do a <- unify t t'
                            b <- unify (substitute a s) (substitute a s')
                            return (a @@ b)
  unify _ _            = fail "unify failed"

  bindvar v@(id,KStack) t = return (v +-> TStack t)
  bindvar _ _             = fail "bindvar failed (kind mismatch)"

instance CanSubstitute Stack where
  substitute s (SPush t h)  = SPush (substitute s t) (substitute s h)
  substitute s (SBottom id) = case lookup (id,KStack) s of
                                 Just (TStack t) -> t
                                 Just t  -> SBottom id
                                 Nothing -> SBottom id
  substitute s t = t

  freeVars (SBottom id) = [(id,KStack)]
  freeVars (SPush t h)  = freeVars t `union` freeVars h
  freeVars _             = []

showStack :: Stack -> String
showStack SEmpty       = "∅"
showStack (SPush s s') = showStack s ++ " " ++ showType s'
showStack (SBottom id) = showVar id KStack
