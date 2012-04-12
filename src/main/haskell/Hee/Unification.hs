module Hee.Unification
  ( CanSubstitute(..)
  , CanUnify(..)
  ) where

import Data.List (nub, intersect, union)
import Hee.Kinds
import Hee.Types

type Id
  = String

type Variable
  = (Id,Kind)

type Substitution
  = [(Variable, Type)]

-- HasKind Variable
instance (HasKind b) => HasKind (a,b) where
  kind (_,k) = kind k

-- Empty subtitution
empty :: Substitution
empty = []

-- Singleton substitution
--   kind preserving iff (kind v) == (kind t)
(+->)    :: Variable -> Type -> Substitution
(+->) v t = [(v, t)]

class CanSubstitute t where
  substitute :: Substitution -> t -> t
  freevars   :: t -> [Variable]

instance CanSubstitute Type where
  substitute s (TyApplication i o) = TyApplication (substitute s i) (substitute s o)
  substitute s (TyStack t)         = TyStack (substitute s t)
  substitute s (TyVariable id k)   = case lookup (id,k) s of
                                       Just t  -> t
                                       Nothing -> TyVariable id k
  substitute s t = t

  freevars (TyApplication i o) = freevars i `union` freevars o
  freevars (TyStack t)         = freevars t
  freevars (TyVariable id k)   = [(id,k)]
  freevars _                   = []

instance CanSubstitute Stack where
  substitute s (StBottom id) = case lookup (id,KiStack) s of
                                 Just (TyStack t) -> t
                                 Just t  -> StBottom id
                                 Nothing -> StBottom id
  substitute s t = t

  freevars (StBottom id) = [(id,KiStack)]
  freevars _             = []

instance CanSubstitute a => CanSubstitute [a] where
  substitute s = map (substitute s)
  freevars     = nub . concat . map freevars

-- Composition of substitutions
--   substitute (a @@ b) = substitute a . substitute b
infixr   4 @@
(@@)    :: Substitution -> Substitution -> Substitution
(@@) a b = [(v, substitute a t) | (v, t) <- b] ++ b

-- Composition of substitutions
merge    :: Monad m => Substitution -> Substitution -> m Substitution
merge a b = if all match (map fst a `intersect` map fst b)
            then return (a ++ b)
            else fail "merge failed"
  where match (id,KiStack) = substitute a (StBottom id)     == substitute b (StBottom id)
        match (id,k)       = substitute a (TyVariable id k) == substitute b (TyVariable id k)

class CanUnify t where
  match   :: Monad m => t -> t -> m Substitution
  unify   :: Monad m => t -> t -> m Substitution
  bindvar :: Monad m => Variable -> t -> m Substitution

instance CanUnify Type where
  match (TyStack s) (TyStack s') = match s s'
  match (TyVariable id k) t      = bindvar (id,k) t
  match (TyConstructor id k) (TyConstructor id' k')
    | id == id' && k == k'       = return empty
  match (TyApplication i o) (TyApplication i' o')
                                 = do a <- match i i'
                                      b <- match (substitute a o) (substitute a o')
                                      merge a b
  match _ _                      = fail "merge failed"

  unify (TyStack s) (TyStack s') = unify s s'
  unify (TyVariable id k) t      = bindvar (id,k) t
  unify t (TyVariable id k)      = bindvar (id,k) t
  unify (TyConstructor id k) (TyConstructor id' k')
    | id == id' && k == k'       = return empty
  unify (TyApplication i o) (TyApplication i' o')
                                 = do a <- unify i i'
                                      b <- unify (substitute a o) (substitute a o')
                                      return (a @@ b)
  unify _ _                      = fail "unify failed"

  bindvar v@(id,k) t
    | t == TyVariable id k = return empty
    | v `elem` freevars t  = fail "bindvar failed (occurs check)"
    | k /= kind t          = fail "bindvar failed (kind mismatch)"
    | otherwise            = return (v +-> t)

instance CanUnify Stack where
  match (StBottom id) t = bindvar (id,KiStack) t
  match StEmpty StEmpty = return empty
  match (StPush t s) (StPush t' s')
                        = do a <- match t t'
                             b <- match (substitute a s) (substitute a s')
                             merge a b

  unify (StBottom id) t = bindvar (id,KiStack) t
  unify t (StBottom id) = bindvar (id,KiStack) t
  unify StEmpty StEmpty = return empty
  unify (StPush t s) (StPush t' s')
                        = do a <- unify t t'
                             b <- unify (substitute a s) (substitute a s')
                             return (a @@ b)
  unify _ _             = fail "unify failed"

  bindvar v@(id,KiStack) t = return (v +-> TyStack t)
  bindvar _ _              = fail "bindvar failed (kind mismatch)"

