module Hee.Substitution
  ( CanSubstitute(..)
  , Substitution
  , Variable
  , empty
  , (+->)
  , (@@)
--, merge
  , freshVars
  , normalizeVars
  , normalizeType
  , splitIds
  , nextFree
  , generalize
  ) where

import Data.List (nub, intersect, union, sort, foldl')
import Hee.Kinds

type Id
  = Int

type Variable
  = (Id,Kind)

type Substitution t
  = [(Variable, t)]

-- HasKind Variable
instance (HasKind b) => HasKind (a,b) where
  kind (_,k) = kind k

-- Empty subtitution
empty :: Substitution t
empty = []

-- Singleton substitution
--   kind preserving iff (kind v) == (kind t)
(+->)    :: Variable -> a -> Substitution a
(+->) v t = [(v, t)]

class CanSubstitute t where
  substitute :: Substitution t -> t -> t
  freeVars   :: t -> [Variable]

instance CanSubstitute a => CanSubstitute [a] where
  substitute s = map (substitute s)
  freeVars     = nub . concat . map freeVars

-- Composition of substitutions
--   substitute (a @@ b) = substitute a . substitute b
infixr   4 @@
(@@)    :: Substitution t -> Substitution t -> Substitution t
(@@) a b = [(v, substitute a t) | (v, t) <- b] ++ a

-- Composition of substitutions
--merge    :: Monad m => Substitution t -> Substitution t -> m (Substitution t)
--merge a b = if all match (map fst a `intersect` map fst b)
--            then return (a ++ b)
--            else fail "merge failed"
--  where match (id,KStack) = substitute a (SBottom id)     == substitute b (SBottom id)
--        match (id,k)      = substitute a (TVariable id k) == substitute b (TVariable id k)

class CanUnify t where
  match   :: Monad m => t -> t -> m (Substitution t)
  unify   :: Monad m => t -> t -> m (Substitution t)
  bindvar :: Monad m => Variable -> t -> m (Substitution t)
