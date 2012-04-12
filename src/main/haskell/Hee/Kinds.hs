module Hee.Kinds
  ( Kind(..)
  , HasKind(..)
  ) where

-- Kinds classify types as either a monomorphic value type (a nullary type
-- constructor), a unary type constructor (type => type), or a stack
data Kind
  = KiStack
  | KiType
  | KiConstructor Kind Kind
  deriving (Eq)

showKind :: Kind -> String
showKind KiStack = "@"
showKind KiType  = "*"
showKind (KiConstructor k@(KiConstructor _ _) k')
                 = "(" ++ showKind k ++ ") => " ++ showKind k'
showKind (KiConstructor k k')
                 = showKind k ++ " => " ++ showKind k'

instance Show Kind where
  show = showKind

class HasKind t where
  kind :: t -> Kind

instance HasKind Kind where
  kind k = k
