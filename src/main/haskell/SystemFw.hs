{-# LANGUAGE UnicodeSyntax #-}

module Hee.SystemFw
  ( Term(..)
  , Type(..)
  , Kind(..)
  , bool
  , tru
  , fls
  , nat
  , zero
  , succ
  , pair
  , fst
  , snd
  , sum
  , inl
  , inr
  , list
  , null
  , cons
  ) where

import Prelude hiding (succ, fst, snd, sum, null)

type Id
  = Int

data Term
  = TmVariable Id                 -- x,y         term variable
  | TmApplication Term Term       -- f e         term application
  | TmAbstraction Id Type Term    -- λx:τ. e     term abstraction
  | TmUAbstraction Id Kind Term   -- Λα:κ. e     universal abstraction
  | TmUApplication Term Type      -- f τ         universal application
  deriving (Eq)

data Type
  = TVariable Id                  -- α,β         type variable
  | TApplication Type Type        -- τ υ         type application
  | TAbstraction Id Kind Type     -- λα:κ. τ     type abstraction
  | TQuantification Id Kind Type  -- ∀α:κ. τ     type quantification
  | TOperator Type Type           -- τ → υ       type of operator on terms
  deriving (Eq)

data Kind
  = KType                         -- ★           kind of manifest type
  | KOperator Kind Kind           -- κ → ι       kind of operator on types
  deriving (Eq)

---------------------------------------------------------------------------

showTmId x = showId x "abcdefghijklmnopqrstuvwxyz"
showTId a  = showId a "αβγδεζηθικλμνξοπρςστυφχψω"

showId id alphabet = (alphabet !! n) : (replicate k '\'')
  where k = id `div` length alphabet
        n = id `mod` length alphabet

showTerm 0 (TmVariable x)         = showTmId x
showTerm 0 (TmUApplication e t)   = showTerm 1 e ++ " " ++ showType 1 t
showTerm 0 (TmApplication e f)    = showTerm 1 e ++ " " ++ showTerm 1 f
showTerm 0 (TmUAbstraction a k e) = "Λ" ++ showTId a  ++ ":" ++ showKind 0 k ++ ". " ++ showTerm 0 e
showTerm 0 (TmAbstraction x t e)  = "λ" ++ showTmId x ++ ":" ++ showType 1 t ++ ". " ++ showTerm 0 e

showTerm n (TmVariable x)         = showTmId x
showTerm n (TmUApplication e t)   = "(" ++ showTerm (n+1) e ++ " " ++ showType (n+1) t ++ ")"
showTerm n (TmApplication e f)    = "(" ++ showTerm (n+1) e ++ " " ++ showTerm (n+1) f ++ ")"
showTerm n (TmUAbstraction a k e) = "(Λ" ++ showTId a  ++ ":" ++ showKind (n+1) k ++ ". " ++ showTerm 0 e ++ ")"
showTerm n (TmAbstraction x t e)  = "(λ" ++ showTmId x ++ ":" ++ showType (n+1) t ++ ". " ++ showTerm 0 e ++ ")"

showType 0 (TVariable a)            = showTId a
showType 0 (TOperator t u)          = showType 1 t ++ " → " ++ showType 1 u
showType 0 (TApplication a b)       = showType 1 a ++ " "   ++ showType 1 b
showType 0 (TAbstraction a k t)     = "λ" ++ showTId a ++ ":" ++ showKind 0 k ++ ". " ++ showType 0 t
showType 0 (TQuantification a k t)  = "∀" ++ showTId a ++ ":" ++ showKind 0 k ++ ". " ++ showType 0 t

showType n (TVariable a)            = showTId a
showType n (TOperator t u)          = "(" ++ showType (n+1) t ++ " → " ++ showType (n+1) u ++ ")"
showType n (TApplication a b)       = "(" ++ showType (n+1) a ++ " "   ++ showType (n+1) b ++ ")"
showType n (TAbstraction a k t)     = "(λ" ++ showTId a ++ ":" ++ showKind (n+1) k ++ ". " ++ showType 0 t ++ ")"
showType n (TQuantification a k t)  = "(∀" ++ showTId a ++ ":" ++ showKind (n+1) k ++ ". " ++ showType 0 t ++ ")"

showKind 0 (KType)          = "★"
showKind 0 (KOperator k l)  = showKind 1 k ++ " → " ++ showKind 1 l

showKind n (KType)          = "★"
showKind n (KOperator k l)  = "(" ++ showKind (n+1) k ++ " → " ++ showKind (n+1) l ++ ")"

instance Show Term where
  show = showTerm 0

instance Show Type where
  show = showType 0

instance Show Kind where
  show = showKind 0

---------------------------------------------------------------------------

-- Bool : ★
-- Bool = ∀α. α → α → α
bool  = TQuantification 0 KType $
          TOperator (TVariable 0) $
            TOperator (TVariable 0) (TVariable 0)

--tru : ∀α. α → α → α
--tru = Λα:*. λt:α. λf:α. t
tru = TmUAbstraction 0 KType $
         TmAbstraction 19 (TVariable 0) $
           TmAbstraction 5 (TVariable 0) $
             TmVariable 19

--fls : ∀α. α → α → α
--fls = Λα:*. λt:α. λf:α. f
fls = TmUAbstraction 0 KType $
        TmAbstraction 19 (TVariable 0) $
          TmAbstraction 5 (TVariable 0) $
            TmVariable 5

-- Nat : ★
-- Nat = ∀α. α → (α → α) → α
nat = TQuantification 0 KType $
        TOperator (TVariable 0) $
          TOperator
            (TOperator (TVariable 0) (TVariable 0))
            (TVariable 0)

-- zero : ∀α. α → (α → α) → α
-- zero = Λα:*. λa:α. λf:α → α. a
zero =  TmUAbstraction 0 KType $
          TmAbstraction 0 (TVariable 0) $
            TmAbstraction 5 (TOperator (TVariable 0) (TVariable 0)) $
              TmVariable 0

-- succ : ∀α. α → (α → α) → α
-- succ = Λα:*. λa:α. λf:α → α. f a
succ =  TmUAbstraction 0 KType $
          TmAbstraction 0 (TVariable 0) $
            TmAbstraction 5 (TOperator (TVariable 0) (TVariable 0)) $
              TmApplication (TmVariable 5) (TmVariable 0)

-- Product : ★ → (★ → ★)
-- Product = λα:*. λβ:*. ∀R:*. (α → β → R) → R
pair =  TAbstraction 0 KType $
          TAbstraction 1 KType $
            TQuantification 15 KType $
              TOperator
                (TOperator (TVariable 0) $
                  TOperator (TVariable 1) (TVariable 15))
                (TVariable 15)

-- fst : ∀α:*. ∀β:*. ∀R:*. (α → β → R) → α
-- fst = Λα:*. Λβ:*. λp:(∀R:*. α → β → R). p α (λa:α. λb:β. a)
fst = TmUAbstraction 0 KType $
        TmUAbstraction 1 KType $
          TmAbstraction 15
            (TQuantification 15 KType
              (TOperator (TVariable 0)
                (TOperator (TVariable 1) (TVariable 15))))
            (TmApplication
              (TmUApplication (TmVariable 15) (TVariable 0))
              (TmAbstraction 0 (TVariable 0)
                (TmAbstraction 1 (TVariable 1)
                  (TmVariable 0))))

-- snd : ∀α:*. ∀β:*. ∀R:*. (α → β → R) → β 
-- snd = Λα:*. Λβ:*. λp:(∀R:*. α → β → R). p β (λa:α. λb:β. b)
snd = TmUAbstraction 0 KType $
        TmUAbstraction 1 KType $
          TmAbstraction 15
            (TQuantification 15 KType
              (TOperator (TVariable 0)
                (TOperator (TVariable 1) (TVariable 15))))
            (TmApplication
              (TmUApplication (TmVariable 15) (TVariable 0))
              (TmAbstraction 0 (TVariable 0)
                (TmAbstraction 1 (TVariable 1)
                  (TmVariable 1))))

-- Sum : ★ → (★ → ★)
-- Sum = λα:*. λβ:*. ∀R:*. (α → R) → (β → R) → R
sum = TAbstraction 0 KType $
        TAbstraction 1 KType $
          TQuantification 2 KType $
            TOperator (TOperator (TVariable 0) (TVariable 2)) $
              TOperator (TOperator (TVariable 1) (TVariable 2)) $
                TVariable 2

-- inL : ∀α:*. ∀β:*. α → (∀R:*. (α → R) → (β → R) → R) → α
-- inL = Λα:*. Λβ:*. ∀R:*. λa:α. λleft:(α → R). λright:(β → R). left a
inl = TmUAbstraction 0 KType $
        TmUAbstraction 1 KType $
          TmUAbstraction 2 KType $
            TmAbstraction 0 (TVariable 0) $
              TmAbstraction 1 (TOperator (TVariable 0) (TVariable 2)) $
                TmAbstraction 2 (TOperator (TVariable 1) (TVariable 2)) $
                  TmApplication (TmVariable 1) (TmVariable 0)

-- inL : ∀α:*. ∀β:*. α → (∀R:*. (α → R) → (β → R) → R) → β
-- inR = Λα:*. Λβ:*. ∀R:*. λv:β. λleft:(α → R). λright:(β → R). right a
inr = TmUAbstraction 0 KType $
        TmUAbstraction 1 KType $
          TmUAbstraction 2 KType $
            TmAbstraction 0 (TVariable 1) $
              TmAbstraction 1 (TOperator (TVariable 0) (TVariable 2)) $
                TmAbstraction 2 (TOperator (TVariable 1) (TVariable 2)) $
                  TmApplication (TmVariable 2) (TmVariable 0)

-- List : ★ → ★
-- List = λα:*. ∀R:*. (α → R → R) → R → R
list =  TAbstraction 0 KType $
          TQuantification 1 KType $
            TOperator
              (TOperator
                (TVariable 0)
                (TOperator (TVariable 1) (TVariable 1)))
              (TOperator (TVariable 1) (TVariable 1))

-- null : ∀α:*. ∀R:*. (α → R → R) → R → R
-- null = Λα:*. ∀R:*. λf:(α → R → R). λr:R. r
null =  TmUAbstraction 0 KType $
          TmUAbstraction 1 KType $
            TmAbstraction 0
              (TOperator (TVariable 0) $
                TOperator (TVariable 1) (TVariable 1))
              (TmAbstraction 1 (TVariable 1) $
                TmVariable 1)

-- cons : ∀α:*. ∀R:*. α → ((α → R → R) → R → R) → ((α → R → R) → R → R)
-- cons = Λα:*. ∀R:*. λhead:α. λtail:(α → R → R) → R → R. λf:(α → R → R). λr:R. f head (tail f r)
cons =  TmUAbstraction 0 KType $
          TmUAbstraction 1 KType $
            TmAbstraction 0 (TVariable 0) $
              TmAbstraction 1
                (TOperator
                  (TOperator (TVariable 0) $
                    TOperator (TVariable 1) (TVariable 1))
                  (TOperator (TVariable 1) (TVariable 1)))
                (TmAbstraction 2
                  (TOperator (TVariable 0) $
                    TOperator (TVariable 1) (TVariable 1))
                  (TmAbstraction 3 (TVariable 1) $
                    TmApplication (TmApplication (TmVariable 2) (TmVariable 0)) $
                      TmApplication (TmApplication (TmVariable 1) (TmVariable 2)) (TmVariable 3)))
