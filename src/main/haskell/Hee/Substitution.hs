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
  substitute s (TyApplication i o) = TyApplication (substitute s i) (substitute s o)
  substitute s (TyStack t)         = TyStack (substitute s t)
  substitute s (TyVariable id k)   = case lookup (id,k) s of
                                       Just t  -> t
                                       Nothing -> TyVariable id k
  substitute s t = t

  freeVars (TyApplication i o) = freeVars i `union` freeVars o
  freeVars (TyStack t)         = freeVars t
  freeVars (TyVariable id k)   = [(id,k)]
  freeVars _                   = []

instance CanSubstitute Stack where
  substitute s (StBottom id) = case lookup (id,KiStack) s of
                                 Just (TyStack t) -> t
                                 Just t  -> StBottom id
                                 Nothing -> StBottom id
  substitute s t = t

  freeVars (StBottom id) = [(id,KiStack)]
  freeVars _             = []

instance CanSubstitute a => CanSubstitute [a] where
  substitute s = map (substitute s)
  freeVars     = nub . concat . map freeVars

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
    | v `elem` freeVars t  = fail "bindvar failed (occurs check)"
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

normalizeVars :: [Variable] -> Substitution
normalizeVars = freshVars []

-- Returns a substitution that renames all variables in gs such that
--   freeVars fs `intersect` freeVars gs == []
freshVars :: [Variable] -> [Variable] -> Substitution
freshVars fs gs = substitution
  where
    boundIds     = splitIds fs
    thd (a,b,c)  = c
    substitution = thd $ foldl' (\(id,(ts,ss),s) g ->
                     case g of
                       (v, KiType)  ->
                         let (id', ts') = nextFree id ts
                          in if v == id'
                             then (id', (ts', ss), s)
                             else (id', (ts', ss), (g, TyVariable id' KiType):s)
                       (v, KiStack) ->
                         let (id', ss') = nextFree id ss
                          in if v == id'
                             then (id', (ts, ss'), s)
                             else (id', (ts, ss'), (g, TyStack $ StBottom id'):s))
                     (-1, boundIds, empty) gs

    -- Split variables into list of KiType ids and KiStack ids
    splitIds :: [Variable] -> ([Id], [Id])
    splitIds = (sort `fmap`) . foldl' (\(ts, ss) x ->
                 case x of
                   (id, KiType)  -> (id:ts, ss)
                   (id, KiStack) -> (ts, id:ss)
                   _             -> (ts, ss)) ([], [])

    nextFree :: Id -> [Id] -> (Id, [Id])
    nextFree current []     = (current+1, [])
    nextFree current (v:vs) = if current+1 < v
                              then (current+1, v:vs)
                              else nextFree v vs
