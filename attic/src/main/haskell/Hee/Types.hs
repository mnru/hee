{-# LANGUAGE FlexibleInstances #-}

module Hee.Types
  where

import Data.List (nub, intersect, union, sort, foldl', (\\))

type Id
  = Int

type Variable
  = (Id,Kind)

type Substitution
  = [(Variable, Type)]

---------------------------------------------------------------------

data Type
  = TVariable Id Kind
  | TConstructor String Kind
  | TApplication Type Type
  | TForall Id Kind Bound Type
  | TQualified [Predicate] Type
  | TStack Stack
  deriving (Eq)

-- List of identity functions:
--   ∀(β≽∀α.α→α).[β] ⊑ [∀α.α→α]
--   ∀(β≽∀α.α→α).[β] ⊑ ∀α.[α→α]
--
-- (⊑) ⊆ (⊧) ⊆ (≡)
--   ≡, equivalence relation
--   ⊧, abstraction relation
--   ⊑, instance relation

data Bound
  = Rigid     -- ∀(α=υ).τ means τ where α is as polymorphic as υ
  | Flexible  -- ∀(α≽υ).τ means τ where α is equal to or is an instance of υ
  | Bottom    -- ∀α.τ     means τ where α is equal to or is an instance of ⊥

data Predicate
  = MemberOf Type
  deriving (Eq)

data Stack
  = SEmpty
  | SBottom Id
  | SPush Stack Type
  deriving (Eq)

data Kind
  = KStack
  | KType
  | KConstructor Kind Kind
  deriving (Eq)

---------------------------------------------------------------------

class HasKind t where
  kind :: t -> Kind

instance HasKind Kind where
  kind k = k

instance HasKind Stack where
  kind t = KStack

instance HasKind Type where
  kind (TVariable _ k)    = k
  kind (TConstructor _ k) = k
  kind (TStack _)         = KStack
  kind (TForall _ _ _ t)  = kind t
  kind (TQualified _ t)   = kind t
  kind (TApplication i _) = let (KConstructor _ k) = kind i in k

instance (HasKind b) => HasKind (a,b) where
  kind (_,k) = kind k

---------------------------------------------------------------------

data EUnify
  = EOccursCheck
  | EKindMismatch
  | EExprMismatch
  deriving (Eq, Show)

instance Monad (Either EUnify) where
  return x      = Right x
  Right r >>= f = f r
  Left l  >>= _ = Left l

class CanUnify t where
  match   :: t -> t -> Either EUnify Substitution
  unify   :: t -> t -> Either EUnify Substitution
  bindvar :: Variable -> t -> Either EUnify Substitution

instance CanUnify Type where
  unify (TStack s) (TStack s') = unify s s'
  unify (TVariable id k) t     = bindvar (id,k) t
  unify t (TVariable id k)     = bindvar (id,k) t
  unify (TConstructor id k) (TConstructor id' k')
    | id == id' && k == k'     = return empty
  unify (TQualified ps t) (TQualified ps' t')
                               = undefined
  unify (TForall id k b t) (TForall id' k' b' t')
                               = undefined
  unify (TApplication i o) (TApplication i' o')
                               = do a <- unify i i'
                                    b <- unify (substitute a o) (substitute a o')
                                    return (a @@ b)
  unify _ _                    = Left EExprMismatch

  match (TStack s) (TStack s') = match s s'
  match (TVariable id k) t     = bindvar (id,k) t
  match (TConstructor id k) (TConstructor id' k')
    | id == id' && k == k'     = return empty
  match (TQualified ps t) (TQualified ps' t')
                               = undefined
  match (TForall id k b t) (TForall id' k' b' t')
                               = undefined
  match (TApplication i o) (TApplication i' o')
                               = do a <- match i i'
                                    b <- match (substitute a o) (substitute a o')
                                    merge a b
  match _ _                    = Left EExprMismatch

  bindvar v@(id,k) t
    | t == TVariable id k = return empty
    | v `elem` freeVars t = Left EOccursCheck
    | k /= kind t         = Left EKindMismatch
    | otherwise           = return (v +-> t)

instance CanUnify Stack where
  match (SBottom id) t = bindvar (id,KStack) t
  match SEmpty SEmpty  = return empty
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
  unify _ _            = Left EExprMismatch

  bindvar v@(id,KStack) t = return (v +-> TStack t)
  bindvar _ _             = Left EOccursCheck

---------------------------------------------------------------------

class CanSubstitute t where
  substitute :: Substitution -> t -> t
  freeVars   :: t -> [Variable]

instance CanSubstitute a => CanSubstitute [a] where
  substitute s = map (substitute s)
  freeVars     = nub . concat . map freeVars

instance CanSubstitute Type where
  substitute s (TApplication i o) = TApplication (substitute s i) (substitute s o)
  substitute s (TStack t)         = TStack (substitute s t)
  substitute s (TForall id k b t) = TForall id k b (substitute (filter (\(v,_) -> (id,k) /= v) s) t)
  substitute s (TQualified ps t)  = TQualified ps (substitute s t)
  substitute s (TVariable id k)   = case lookup (id,k) s of
                                      Just t  -> t
                                      Nothing -> TVariable id k
  substitute _ t                  = t

  freeVars (TApplication i o) = freeVars i `union` freeVars o
  freeVars (TStack t)         = freeVars t
  freeVars (TVariable id k)   = [(id,k)]
  freeVars (TForall id k b t) = freeVars t \\ [(id,k)]
  freeVars (TQualified ps t)  = freeVars t
  freeVars _                  = []

instance CanSubstitute Stack where
  substitute s (SPush t h)  = SPush (substitute s t) (substitute s h)
  substitute s (SBottom id) = case lookup (id,KStack) s of
                                 Just (TStack t) -> t
                                 Just t  -> SBottom id
                                 Nothing -> SBottom id
  substitute _ t            = t

  freeVars (SBottom id) = [(id,KStack)]
  freeVars (SPush t h)  = freeVars t `union` freeVars h
  freeVars _            = []

---------------------------------------------------------------------

-- Composition of substitutions
merge :: Substitution -> Substitution -> Either EUnify Substitution
merge a b = if all match (map fst a `intersect` map fst b)
            then return (a ++ b)
            else Left EExprMismatch
  where match (id,KStack) = substitute a (SBottom id)     == substitute b (SBottom id)
        match (id,k)      = substitute a (TVariable id k) == substitute b (TVariable id k)

-- Composition of substitutions
--   substitute (a @@ b) = substitute a . substitute b
infixr 4 @@
(@@) :: Substitution -> Substitution -> Substitution
(@@) a b = [(v, substitute a t) | (v, t) <- b] ++ a

-- Empty subtitution
empty :: Substitution
empty = []

-- Singleton substitution
--   kind preserving iff (kind v) == (kind t)
(+->) :: Variable -> Type -> Substitution
(+->) v t = [(v, t)]

---------------------------------------------------------------------
