module Language.Hee.Syntax
  ( Declaration(..)
  , Literal(..)
  , Kind(..)
  , Radix(..)
  , Expr(..)
  , Type(..)
  ) where

import Data.Text

data Expr
  = ExEmpty
  | ExName Text
  | ExQuote Expr
  | ExLiteral Literal
  | ExCompose Expr Expr
  | ExAnnotate Expr Type
  | ExComment Text
  deriving (Eq, Show)

data Radix
  = Binary
  | Octal
  | Decimal
  | Hexadecimal
  deriving (Eq, Show)

data Literal
  = LiChar Char
  | LiString Text
  | LiNumber Radix Int
  deriving (Eq, Show)

data Declaration
  = DeWord
  | DeType
  | DeModule
  deriving (Eq, Show)

data Type
  = Null
  deriving (Eq, Show)

data Kind
  = KStack                  -- kind of a stack
  | KType                   -- kind of a base type
  | KConstructor Kind Kind  -- kind of a type constructor
  deriving (Eq, Show)
