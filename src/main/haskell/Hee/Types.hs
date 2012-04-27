module Hee.Types
  ( Type(..)
  , Stack(..)
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
  , showStack
  ) where

import Hee.Kinds

type Id
  = Int

-- Types have the kind KType and are distinguished from Stack because
-- they can be used to describe first-class values.
data Type
  = TVariable Id Kind
  | TConstructor String Kind
  | TApplication Type Type
  | TStack Stack
  deriving (Eq, Show)

-- Stacks have the kind KStack and are distinguished from Type because
-- they cannot be used to describe first-class values.
data Stack
  = SEmpty
  | SBottom Id
  | SPush Stack Type
  deriving (Eq, Show)

-- t ∈ Eq
-- t ∈ Num
data Predicate
  = MemberOf Id Type
  deriving (Eq, Show)

-- Qualified types antecedents => consequent
data Qualified h
  = [Predicate] :=> h
  deriving (Eq, Show)

showId id alphabet =(alphabet !! n) : (replicate k '\'')
  where k = id `div` length alphabet
        n = id `mod` length alphabet

showType :: Type -> String
showType (TConstructor id k) = id
showType (TVariable id k)    = showId id "abcdefghijklmnopqrtsuvwxyz"
showType (TApplication (TApplication f i) o) | f == tFunc = "(" ++ showType i ++ " → " ++ showType o ++ ")"
showType (TApplication (TApplication f i) o) | f == tPair = "(" ++ showType i ++ "," ++ showType o ++ ")"
showType (TApplication f i)                  | f == tList = "[" ++ showType i ++ "]"
showType (TApplication f x)                               = "(" ++ showType f ++ " " ++ showType x ++ ")"
showType (TStack s) =
  case s of
    SEmpty      -> showStack s
    (SBottom _) -> showStack s
    _            -> showStack s

showStack :: Stack -> String
showStack SEmpty       = "∅"
showStack (SPush s s') = showStack s ++ " " ++ showType s'
showStack (SBottom id) = showId id "ABCDEFGHIJKLMNOPQRTSUVWXYZ"

-- Primitive types
tInt    = TConstructor "int"    KType  -- LiInt
tRatn   = TConstructor "ratn"   KType  -- LiRatn
tChar   = TConstructor "char"   KType  -- LiChar
tBool   = TConstructor "bool"   KType
tString = TConstructor "string" KType  -- LiString

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

-- TODO: Free type variable
quote :: Type -> Type
quote t = (SBottom 0) `mkFunc` (SPush (SBottom  0) t)

--instance Show Type where
--  show = showType
--
--instance Show Stack where
--  show = showStack

instance HasKind Type where
  kind (TVariable _ k)     = k
  kind (TConstructor _ k)  = k
  kind (TStack _)          = KStack
  kind (TApplication i _)  = let (KConstructor _ k) = kind i in k

instance HasKind Stack where
  kind t = KStack
