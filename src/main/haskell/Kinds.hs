module Hee.Kinds where

-- Kinds classify types as either a monomorphic value type (a nullary type
-- constructor), a unary type constructor (type => type), or a stack
data Kind
  = KiStack
  | KiType
  | KiCons Kind Kind
  deriving (Eq, Show)
