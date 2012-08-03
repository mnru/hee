module Language.Hee.Syntax
  ( Expression(..)
  , Radix(..)
  , Literal(..)
  , Kind(..)
  , Stack(..)
  , Id
  , Type(..)
  , Declaration(..)
  ) where

import Data.Text

data Expression
  = EEmpty
  | EName Text
  | EQuote Expression
  | ELiteral Literal
  | ECompose Expression Expression
  | EAnnotate Expression Type
  | EComment Text
  deriving (Eq, Show)

data Radix
  = Binary
  | Octal
  | Decimal
  | Hexadecimal
  deriving (Eq, Show)

data Literal
  = LChar Char
  | LString Text
  | LNumber Radix Int
  deriving (Eq, Show)

data Kind
  = KStack                  -- kind of a stack
  | KType                   -- kind of a base type
  | KConstructor Kind Kind  -- kind of a type constructor
  deriving (Eq, Show)

type Id
  = Int

type Variable
  = (Id, Kind)

data Stack
  = SEmpty
  | STail Int
  | SPush Type Stack
  deriving (Eq, Show)

data Type
  = TStack Stack
  | TConstructor Text Kind
  | TApplication Type Type
  | TForall Id Kind Type
  deriving (Eq, Show)

data Declaration
  = DWord
  | DType
  | DModule
  deriving (Eq, Show)
