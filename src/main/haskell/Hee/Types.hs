module Hee.Types
  ( Type(..)
  , Stack(..)
  , tInt
  , tRatn
  , tChar
  , tPair
  , tList
  , tFunc
  , tString
  , mkFunc
  , mkList
  , mkVar
  , showType
  , showStack
  ) where

import Hee.Kinds

type Id
  = String

-- Types have the kind KiType and are distinguished from Stack because
-- they can be used to describe first-class values.
data Type
  = TyVariable Id Kind
  | TyConstant Id Kind
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

showType :: Type -> String
showType (TyVariable id k)   = id
showType (TyConstant id k)   = id
showType (TyApplication (TyApplication f i) o)
  | f == tFunc               = "(" ++ showType i ++ " -> " ++ showType o ++ ")"
showType (TyApplication f x) = showType f ++ " " ++ showType x
showType (TyStack s) =
  case s of
    StEmpty      -> showStack s
    (StBottom _) -> showStack s
    _            -> showStack s

showStack :: Stack -> String
showStack StEmpty       = "|"
showStack (StBottom id) = id
showStack (StPush s s') = showStack s ++ " " ++ showType s'

-- Primitive types
tInt    = TyConstant "int"    KiType  -- LiInt
tRatn   = TyConstant "ratn"   KiType  -- LiRatn
tChar   = TyConstant "char"   KiType  -- LiChar
tString = TyConstant "string" KiType  -- LiString

-- Composite types
tPair   = TyConstant "(,)"    (KiCons KiType (KiCons KiType KiType))
tList   = TyConstant "[]"     (KiCons KiType KiType)
tFunc   = TyConstant "(->)"   (KiCons KiStack KiStack)

mkVar :: String -> Type
mkVar id = TyVariable id KiType

mkFunc :: Stack -> Stack -> Type
mkFunc inp out = TyApplication (TyApplication tFunc (TyStack inp)) (TyStack out)

mkList :: Type -> Type
mkList t = TyApplication tList t

mkPair :: Type -> Type -> Type
mkPair fst snd = TyApplication (TyApplication tPair fst) snd

--instance Show Type where
--  show = showType

--instance Show Stack where
--  show = showStack

instance HasKind Type where
  kind (TyVariable _ k)     = k
  kind (TyConstant _ k)     = k
  kind (TyStack _)          = KiStack
  kind (TyApplication i _)  = let (KiCons _ k) = kind i in k

instance HasKind Stack where
  kind t = KiStack
