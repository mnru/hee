module Hee.Substitution
  ( CanSubstitute(..)
  , Substitution
  , Variable
  , empty
  , (+->)
  , (@@)
  , merge
  , freshVars
  , normalizeVars
  , normalizeType
  , splitIds
  , nextFree
  ) where

import Data.List (nub, intersect, union, sort, foldl')
import Hee.Kinds
import Hee.Types

type Id
  = Int

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
  freeVars   :: t -> [Variable]

instance CanSubstitute Type where
  substitute s (TApplication i o) = TApplication (substitute s i) (substitute s o)
  substitute s (TStack t)         = TStack (substitute s t)
  substitute s (TVariable id k)   = case lookup (id,k) s of
                                      Just t  -> t
                                      Nothing -> TVariable id k
  substitute s t = t

  freeVars (TApplication i o) = freeVars i `union` freeVars o
  freeVars (TStack t)         = freeVars t
  freeVars (TVariable id k)   = [(id,k)]
  freeVars _                  = []

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

instance CanSubstitute a => CanSubstitute [a] where
  substitute s = map (substitute s)
  freeVars     = nub . concat . map freeVars

-- Composition of substitutions
--   substitute (a @@ b) = substitute a . substitute b
infixr   4 @@
(@@)    :: Substitution -> Substitution -> Substitution
(@@) a b = [(v, substitute a t) | (v, t) <- b] ++ a

-- Composition of substitutions
merge    :: Monad m => Substitution -> Substitution -> m Substitution
merge a b = if all match (map fst a `intersect` map fst b)
            then return (a ++ b)
            else fail "merge failed"
  where match (id,KStack) = substitute a (SBottom id)     == substitute b (SBottom id)
        match (id,k)       = substitute a (TVariable id k) == substitute b (TVariable id k)

class CanUnify t where
  match   :: Monad m => t -> t -> m Substitution
  unify   :: Monad m => t -> t -> m Substitution
  bindvar :: Monad m => Variable -> t -> m Substitution

instance CanUnify Type where
  match (TStack s) (TStack s') = match s s'
  match (TVariable id k) t     = bindvar (id,k) t
  match (TConstructor id k) (TConstructor id' k')
    | id == id' && k == k'     = return empty
  match (TApplication i o) (TApplication i' o')
                               = do a <- match i i'
                                    b <- match (substitute a o) (substitute a o')
                                    merge a b
  match _ _                    = fail "merge failed"

  unify (TStack s) (TStack s') = unify s s'
  unify (TVariable id k) t     = bindvar (id,k) t
  unify t (TVariable id k)     = bindvar (id,k) t
  unify (TConstructor id k) (TConstructor id' k')
    | id == id' && k == k'     = return empty
  unify (TApplication i o) (TApplication i' o')
                               = do a <- unify i i'
                                    b <- unify (substitute a o) (substitute a o')
                                    return (a @@ b)
  unify _ _                    = fail "unify failed"

  bindvar v@(id,k) t
    | t == TVariable id k = return empty
    | v `elem` freeVars t  = fail "bindvar failed (occurs check)"
    | k /= kind t          = fail "bindvar failed (kind mismatch)"
    | otherwise            = return (v +-> t)

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

normalizeVars :: [Variable] -> Substitution
normalizeVars = freshVars []

normalizeType :: (CanUnify a, CanSubstitute a) => a -> a
normalizeType t = substitute (normalizeVars $ freeVars t) t

-- Returns a substitution that renames all variables in gs such that
--   freeVars fs `intersect` freeVars gs == []
freshVars :: [Variable] -> [Variable] -> Substitution
freshVars fs xs = thd types ++ thd stacks
  where
    thd (a,b,c) = c
    (tbound,sbound) = splitIds fs

    types :: (Id, [Id], Substitution)
    types  = foldl' (\(current, bound, sub) var ->
                      case var of
                        x@(_,KType) ->
                          let (current', bound') = nextFree current bound
                           in (current', bound', (x, TVariable current' KType):sub)
                        _ -> (current, bound, sub))
                    (-1, tbound, empty) xs

    stacks :: (Id, [Id], Substitution)
    stacks = foldl' (\(current, bound, sub) var ->
                      case var of
                        x@(_,KStack) ->
                          let (current', bound') = nextFree current bound
                           in (current', bound', (x, TStack $ SBottom current'):sub)
                        _ -> (current, bound, sub))
                    (-1, sbound, empty) xs

-- Split variables into list of KType ids and KStack ids
splitIds :: [Variable] -> ([Id], [Id])
splitIds vars = (sort ts, sort ss)
  where (ts,ss) = foldl' (\(ts, ss) x ->
                    case x of
                      (id,KType)  -> (id:ts, ss)
                      (id,KStack) -> (ts, id:ss)
                      _             -> (ts, ss)) ([], []) vars

nextFree :: Id -> [Id] -> (Id, [Id])
nextFree current []     = (current+1, [])
nextFree current (v:vs) = if current+1 < v
                          then (current+1, v:vs)
                          else nextFree v vs
