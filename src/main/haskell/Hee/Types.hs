module Hee.Types
  ( Type(..)
  , tInt
  , tRatn
  , tChar
  , tPair
  , tList
  , tFunc
  , tBool
  , tString
  , mkFunc
  , mkList
  , mkVar
  , quote
  , showType
  ) where

import Hee.Kinds
import Hee.Stack
import Hee.Substitution
import Hee.Unification

type Id
  = Int

-- Types have the kind KType and are distinguished from Stack because
-- they can be used to describe first-class values.
data Type
  = TVariable Id Kind
  | TConstructor String Kind
  | TApplication Type Type
  | TForall Id Kind [Predicate] Type
  | TStack Stack
  deriving (Eq, Show)

data Predicate
  = MemberOf Type
  deriving (Eq, Show)

showId :: Id -> String -> String
showId id alphabet = (alphabet !! n) : (replicate k '\'')
  where k = id `div` length alphabet
        n = id `mod` length alphabet

showVar :: Id -> Kind -> String
showVar id KStack = showId id "ABCDEFGHIJKLMNOPQRTSUVWXYZ"
showVar id _      = showId id "abcdefghijklmnopqrtsuvwxyz"

showType :: Type -> String
showType (TStack s)          = showStack s
showType (TConstructor id k) = id
showType (TVariable id k)    = showVar id k
showType (TForall id k ps t) = "∀" ++ (showVar id k) ++ ". " ++ showType t
showType (TApplication (TApplication f i) o) | f == tFunc = "(" ++ showType i ++ " → " ++ showType o ++ ")"
showType (TApplication (TApplication f i) o) | f == tPair = "(" ++ showType i ++ ", " ++ showType o ++ ")"
showType (TApplication f i)                  | f == tList = "[" ++ showType i ++ "]"
showType (TApplication f x)                               = "(" ++ showType f ++ " " ++ showType x ++ ")"

-- Primitive types
tInt    = TConstructor "int"    KType
tRatn   = TConstructor "ratn"   KType
tChar   = TConstructor "char"   KType
tBool   = TConstructor "bool"   KType
tString = TConstructor "string" KType

-- Composite types
tPair   = TConstructor "(,)"  (KConstructor KType (KConstructor KType KType))
tFunc   = TConstructor "(->)" (KConstructor KStack (KConstructor KStack KType))
tList   = TConstructor "[]"   (KConstructor KType KType)

mkVar :: Id -> Type
mkVar id = TVariable id KType

mkFunc :: Stack -> Stack -> Type
mkFunc inp out = TApplication (TApplication tFunc (TStack inp)) (TStack out)

mkList :: Type -> Type
mkList t = TApplication tList t

mkPair :: Type -> Type -> Type
mkPair fst snd = TApplication (TApplication tPair fst) snd

instance HasKind Type where
  kind (TVariable _ k)     = k
  kind (TConstructor _ k)  = k
  kind (TStack _)          = KStack
  kind (TApplication i _)  = let (KConstructor _ k) = kind i in k

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
