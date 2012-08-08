module Language.Hee.Syntax
  ( Expression(..)
  , Radix(..)
  , Literal(..)
  , Kind(..)
  , Stack(..)
  , Id
  , Type(..)
  , Variable(..)
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

data Variable
  = Variable Id Kind
  deriving (Eq, Show)

data Stack
  = SEmpty
  | STail Id
  | SPush Type Stack
  deriving (Eq, Show)

data Type
  = TStack Stack
  | TConstructor Text Kind
  | TApplication Type Type
  | TForall Variable Bound Type
  | TQualified [Predicate] Type
  | TVariable Variable
  deriving (Eq, Show)

-- List of identity functions:
--   ∀(β≽∀α.α→α).[β] ⊑ [∀α.α→α]
--   ∀(β≽∀α.α→α).[β] ⊑ ∀α.[α→α]
--
-- (⊑) ⊆ (⊧) ⊆ (≡)
--   ≡, equivalence relation
--   ⊧, abstraction relation
--   ⊑, instance relation

data Bound
  = Rigid     -- ∀(α=υ).τ means τ where α is as polymorphic as υ
  | Flexible  -- ∀(α≽υ).τ means τ where α is equal to or is an instance of υ
  | Bottom    -- ∀α.τ     means τ where α is equal to or is an instance of ⊥
  deriving (Eq, Show)

data Predicate
  = MemberOf Type
  deriving (Eq, Show)
