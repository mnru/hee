module Hee.Kinds where

-- Kinds classify types as either a monomorphic value type (a nullary type
-- constructor), a unary type constructor (type => type), or a stack
data Kind
  = KiStack
  | KiType
  | KiCons Kind Kind
  deriving (Eq)

showKind :: Kind -> String
showKind KiStack                    = "@"
showKind KiType                     = "*"
showKind (KiCons k@(KiCons _ _) k') = "(" ++ showKind k ++ ") => " ++ showKind k'
showKind (KiCons k k')              = showKind k ++ " => " ++ showKind k'

instance Show Kind where
  show = showKind

