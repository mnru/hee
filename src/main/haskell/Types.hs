module Hee.Types
  ( Type(..)
  , Stack(..)
  , tInt
  , tReal
  , tChar
  , tUnit
  , tPair
  , tList
  , tFunc
  , tString
  , mkFunc
  , mkList
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
  deriving (Eq)

-- Stacks have the kind KiStack and are distinguished from Type because
-- they cannot be used to describe first-class values.
data Stack
  = StEmpty
  | StBottom Id
  | StPush Stack Type
  deriving (Eq)

showType :: Type -> String
showType (TyVariable id k)    = id
showType (TyConstant id k)    = id
showType (TyApplication t t') = "(" ++ showType t ++ ") " ++ showType t'
showType (TyStack s)          = "(" ++ showStack s ++ ")"

showStack :: Stack -> String
showStack StEmpty       = "|"
showStack (StBottom id) = id
showStack (StPush s s') = showStack s ++ "> " ++ showType s'

instance Show Type where
  show = showType

instance Show Stack where
  show = showStack

-- Primitive types
tInt    = TyConstant "int"  KiType
tReal   = TyConstant "real" KiType
tChar   = TyConstant "char" KiType
tUnit   = TyConstant "()"   KiType
tPair   = TyConstant "(,)"  (KiCons KiType (KiCons KiType KiType))
tList   = TyConstant "[]"   (KiCons KiType KiType)
tFunc   = TyConstant "(->)" (KiCons KiStack KiStack)
tString = mkList tChar

mkFunc :: Stack -> Stack -> Type
mkFunc inp out = TyApplication (TyApplication tFunc (TyStack inp)) (TyStack out)

mkList :: Type -> Type
mkList t = TyApplication tList t

mkPair :: Type -> Type -> Type
mkPair fst snd = TyApplication (TyApplication tPair fst) snd

instance HasKind Type where
  kind (TyVariable _ k)     = k
  kind (TyConstant _ k)     = k
  kind (TyStack _)          = KiStack
  kind (TyApplication i _)  = let (KiCons _ k) = kind i in k

instance HasKind Stack where
  kind t = KiStack
