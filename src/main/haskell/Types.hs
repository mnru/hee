module Hee.Types where

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
