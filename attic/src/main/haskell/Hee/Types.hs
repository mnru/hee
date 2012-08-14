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

-- Types have the kind KiType and are distinguished from Stack because
-- they can be used to describe first-class values.
data Type
  = TyVariable Id Kind
  | TyConstructor String Kind
  | TyApplication Type Type
  | TyGeneric Int
  | TyStack Stack
  deriving (Eq, Show)

-- Stacks have the kind KiStack and are distinguished from Type because
-- they cannot be used to describe first-class values.
data Stack
  = StEmpty
  | StBottom Id
  | StPush Stack Type
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
showType (TyConstructor id k) = id
showType (TyApplication (TyApplication f i) o)
  | f == tFunc                = "(" ++ showType i ++ " -> " ++ showType o ++ ")"
showType (TyApplication f x)  = "(" ++ showType f ++ " " ++ showType x ++ ")"
showType (TyStack s) =
  case s of
    StEmpty      -> showStack s
    (StBottom _) -> showStack s
    _            -> showStack s
showType (TyVariable id k)    = showId id alphabet
  where
    alphabet = "abcdefghijklmnopqrtsuvwxyz"

showStack :: Stack -> String
showStack StEmpty       = "|"
showStack (StPush s s') = showStack s ++ " " ++ showType s'
showStack (StBottom id) = showId id alphabet
  where alphabet = "ABCDEFGHIJKLMNOPQRTSUVWXYZ"

-- Primitive types
tInt    = TyConstructor "int"    KiType  -- LiInt
tRatn   = TyConstructor "ratn"   KiType  -- LiRatn
tChar   = TyConstructor "char"   KiType  -- LiChar
tBool   = TyConstructor "bool"   KiType
tString = TyConstructor "string" KiType  -- LiString

-- Composite types
tPair   = TyConstructor "(,)"    (KiConstructor KiType (KiConstructor KiType KiType))
tList   = TyConstructor "[]"     (KiConstructor KiType KiType)
tFunc   = TyConstructor "(->)"   (KiConstructor KiStack (KiConstructor KiStack KiType))

mkVar :: Id -> Type
mkVar id = TyVariable id KiType

mkFunc :: Stack -> Stack -> Type
mkFunc inp out = TyApplication (TyApplication tFunc (TyStack inp)) (TyStack out)

mkList :: Type -> Type
mkList t = TyApplication tList t

mkPair :: Type -> Type -> Type
mkPair fst snd = TyApplication (TyApplication tPair fst) snd

-- TODO: Free type variable
quote :: Type -> Type
quote t = (StBottom 0) `mkFunc` (StPush (StBottom  0) t)

--instance Show Type where
--  show = showType
--
--instance Show Stack where
--  show = showStack

instance HasKind Type where
  kind (TyVariable _ k)     = k
  kind (TyConstructor _ k)  = k
  kind (TyStack _)          = KiStack
  kind (TyApplication i _)  = let (KiConstructor _ k) = kind i in k

instance HasKind Stack where
  kind t = KiStack
