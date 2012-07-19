module Language.Hee.Types
  ( Type
  , Kind
  ) where

data Type
  = Null
  deriving (Eq, Show)

data Kind
  = KStack                  -- kind of a stack
  | KType                   -- kind of a base type
  | KConstructor Kind Kind  -- kind of a type constructor
  deriving (Eq, Show)
