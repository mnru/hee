{-# LANGUAGE UnicodeSyntax #-}

module Hee.SystemFw
  ( Term(..)
  , Type(..)
  , Kind(..)
  , EUnify(..)
  , CanSubstitute
  , CanUnify
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
  , unbindvar
  ) where

import Prelude hiding (succ, fst, snd, sum, null)
import Data.List (union, (\\))

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
  = TVariable Id Kind             -- α,β         type variable
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

class HasKind a where
  kind :: a -> Kind

instance HasKind Type where
  kind (TVariable _ k)         = k
  kind (TOperator t u)         = KType
  kind (TQuantification _ _ t) = kind t
  kind (TAbstraction _ k t)    = KOperator k (kind t)
  kind (TApplication t _)      = let (KOperator k l) = kind t in l

instance HasKind Kind where
  kind = id

---------------------------------------------------------------------------

type Variable       = (Id, Kind)
type Substitution a = [(Variable, a)]

unbindvar :: Variable -> Substitution a -> Substitution a
unbindvar v s = filter (\(w, _) -> w /= v) s

class CanSubstitute t where
  substitute :: Substitution t -> t -> t
  freevars   :: t -> [Variable]

instance CanSubstitute Type where
  substitute s (TOperator t u)          = TOperator (substitute s t) (substitute s u)
  substitute s (TQuantification a k t)  = TQuantification a k (substitute (unbindvar (a, k) s) t)
  substitute s (TAbstraction a k t)     = TAbstraction a k (substitute (unbindvar (a, k) s) t)
  substitute s (TApplication t u)       = TApplication (substitute s t) (substitute s u)
  substitute s (TVariable a k)          = case lookup (a, k) s of
                                            Just t  -> t
                                            Nothing -> TVariable a k

  freevars (TVariable a k)         = [(a, k)]
  freevars (TOperator t u)         = freevars t `union` freevars u
  freevars (TQuantification a k t) = freevars t \\ [(a, k)]
  freevars (TAbstraction a k t)    = freevars t \\ [(a, k)]
  freevars (TApplication t u)      = freevars t `union` freevars u

---------------------------------------------------------------------------

instance Monad (Either a) where
  return x        = Right x
  (Left x) >>= f  = Left x
  (Right x) >>= f = f x

data EUnify t
  = EOccursCheck t t
  | EKindMismatch t t
  | ETypeMismatch t t
  deriving (Eq, Show)

class CanUnify t where
  unify       :: t -> t -> Either (EUnify t) (Substitution t)
  instantiate :: t -> t -> Either (EUnify t) (Substitution t)
  bindvar     :: Variable -> t -> Either (EUnify t) (Substitution t)

instance CanUnify Type where
  unify (TVariable a k) t = bindvar (a, k) t
  unify t (TVariable a k) = bindvar (a, k) t
  unify (TOperator t u) (TOperator t' u') = undefined
  unify (TApplication t u) (TApplication t' v') = undefined
  unify (TAbstraction a k t) (TAbstraction b k' t') = undefined
  unify (TQuantification a k t) (TQuantification b k' t') = undefined
  unify t u = Left (ETypeMismatch t u)

  instantiate (TVariable a k) t = bindvar (a, k) t
  instantiate (TOperator t u) (TOperator t' u')
    | kind t /= kind t' = Left (EKindMismatch t t')
    | kind u /= kind u' = Left (EKindMismatch u u')
    | otherwise         = instantiate t t' >>= \s -> instantiate (substitute s u) (substitute s u')
  instantiate (TApplication t u) (TApplication t' u')
    | kind t /= kind t' = Left (EKindMismatch t t')
    | kind u /= kind u' = Left (EKindMismatch u u')
    | otherwise         = instantiate t t' >>= \s -> instantiate (substitute s u) (substitute s u')
  instantiate u@(TAbstraction a k t) u'@(TAbstraction b k' t')
    | kind k /= kind k' = Left (EKindMismatch u u')
    | otherwise         = instantiate t t'
  instantiate u@(TQuantification a k t) u'@(TQuantification b k' t')
    | kind k /= kind k' = Left (EKindMismatch u u')
    | otherwise         = undefined
  instantiate t u = Left (ETypeMismatch t u)

  bindvar v@(a, k) t
    | v `elem` freevars t = Left (EOccursCheck (TVariable a k) t)
    | k /= kind t         = Left (EKindMismatch (TVariable a k) t)
    | t == TVariable a k  = return []
    | otherwise           = return [(v, t)]

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

showType 0 (TVariable a _)          = showTId a
showType 0 (TOperator t u)          = showType 1 t ++ " → " ++ showType 1 u
showType 0 (TApplication a b)       = showType 1 a ++ " "   ++ showType 1 b
showType 0 (TAbstraction a k t)     = "λ" ++ showTId a ++ ":" ++ showKind 0 k ++ ". " ++ showType 0 t
showType 0 (TQuantification a k t)  = "∀" ++ showTId a ++ ":" ++ showKind 0 k ++ ". " ++ showType 0 t

showType n (TVariable a _)          = showTId a
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
          TOperator (TVariable 0 KType) $
            TOperator (TVariable 0 KType) (TVariable 0 KType)

--tru : ∀α. α → α → α
--tru = Λα:*. λt:α. λf:α. t
tru = TmUAbstraction 0 KType $
         TmAbstraction 19 (TVariable 0 KType) $
           TmAbstraction 5 (TVariable 0 KType) $
             TmVariable 19

--fls : ∀α. α → α → α
--fls = Λα:*. λt:α. λf:α. f
fls = TmUAbstraction 0 KType $
        TmAbstraction 19 (TVariable 0 KType) $
          TmAbstraction 5 (TVariable 0 KType) $
            TmVariable 5

-- Nat : ★
-- Nat = ∀α. α → (α → α) → α
nat = TQuantification 0 KType $
        TOperator (TVariable 0 KType) $
          TOperator
            (TOperator (TVariable 0 KType) (TVariable 0 KType))
            (TVariable 0 KType)

-- zero : ∀α. α → (α → α) → α
-- zero = Λα:*. λa:α. λf:α → α. a
zero =  TmUAbstraction 0 KType $
          TmAbstraction 0 (TVariable 0 KType) $
            TmAbstraction 5 (TOperator (TVariable 0 KType) (TVariable 0 KType)) $
              TmVariable 0

-- succ : ∀α. α → (α → α) → α
-- succ = Λα:*. λa:α. λf:α → α. f a
succ =  TmUAbstraction 0 KType $
          TmAbstraction 0 (TVariable 0 KType) $
            TmAbstraction 5 (TOperator (TVariable 0 KType) (TVariable 0 KType)) $
              TmApplication (TmVariable 5) (TmVariable 0)

-- Product : ★ → (★ → ★)
-- Product = λα:*. λβ:*. ∀R:*. (α → β → R) → R
pair =  TAbstraction 0 KType $
          TAbstraction 1 KType $
            TQuantification 15 KType $
              TOperator
                (TOperator (TVariable 0 KType) $
                  TOperator (TVariable 1 KType) (TVariable 15 KType))
                (TVariable 15 KType)

-- fst : ∀α:*. ∀β:*. ∀R:*. (α → β → R) → α
-- fst = Λα:*. Λβ:*. λp:(∀R:*. α → β → R). p α (λa:α. λb:β. a)
fst = TmUAbstraction 0 KType $
        TmUAbstraction 1 KType $
          TmAbstraction 15
            (TQuantification 15 KType
              (TOperator (TVariable 0 KType)
                (TOperator (TVariable 1 KType) (TVariable 15 KType))))
            (TmApplication
              (TmUApplication (TmVariable 15) (TVariable 0 KType))
              (TmAbstraction 0 (TVariable 0 KType)
                (TmAbstraction 1 (TVariable 1 KType)
                  (TmVariable 0))))

-- snd : ∀α:*. ∀β:*. ∀R:*. (α → β → R) → β 
-- snd = Λα:*. Λβ:*. λp:(∀R:*. α → β → R). p β (λa:α. λb:β. b)
snd = TmUAbstraction 0 KType $
        TmUAbstraction 1 KType $
          TmAbstraction 15
            (TQuantification 15 KType
              (TOperator (TVariable 0 KType)
                (TOperator (TVariable 1 KType) (TVariable 15 KType))))
            (TmApplication
              (TmUApplication (TmVariable 15) (TVariable 0 KType))
              (TmAbstraction 0 (TVariable 0 KType)
                (TmAbstraction 1 (TVariable 1 KType)
                  (TmVariable 1))))

-- Sum : ★ → (★ → ★)
-- Sum = λα:*. λβ:*. ∀R:*. (α → R) → (β → R) → R
sum = TAbstraction 0 KType $
        TAbstraction 1 KType $
          TQuantification 2 KType $
            TOperator (TOperator (TVariable 0 KType) (TVariable 2 KType)) $
              TOperator (TOperator (TVariable 1 KType) (TVariable 2 KType)) $
                TVariable 2 KType

-- inL : ∀α:*. ∀β:*. α → (∀R:*. (α → R) → (β → R) → R) → α
-- inL = Λα:*. Λβ:*. ∀R:*. λa:α. λleft:(α → R). λright:(β → R). left a
inl = TmUAbstraction 0 KType $
        TmUAbstraction 1 KType $
          TmUAbstraction 2 KType $
            TmAbstraction 0 (TVariable 0 KType) $
              TmAbstraction 1 (TOperator (TVariable 0 KType) (TVariable 2 KType)) $
                TmAbstraction 2 (TOperator (TVariable 1 KType) (TVariable 2 KType)) $
                  TmApplication (TmVariable 1) (TmVariable 0)

-- inL : ∀α:*. ∀β:*. α → (∀R:*. (α → R) → (β → R) → R) → β
-- inR = Λα:*. Λβ:*. ∀R:*. λv:β. λleft:(α → R). λright:(β → R). right a
inr = TmUAbstraction 0 KType $
        TmUAbstraction 1 KType $
          TmUAbstraction 2 KType $
            TmAbstraction 0 (TVariable 1 KType) $
              TmAbstraction 1 (TOperator (TVariable 0 KType) (TVariable 2 KType)) $
                TmAbstraction 2 (TOperator (TVariable 1 KType) (TVariable 2 KType)) $
                  TmApplication (TmVariable 2) (TmVariable 0)

-- List : ★ → ★
-- List = λα:*. ∀R:*. (α → R → R) → R → R
list =  TAbstraction 0 KType $
          TQuantification 1 KType $
            TOperator
              (TOperator
                (TVariable 0 KType)
                (TOperator (TVariable 1 KType) (TVariable 1 KType)))
              (TOperator (TVariable 1 KType) (TVariable 1 KType))

-- null : ∀α:*. ∀R:*. (α → R → R) → R → R
-- null = Λα:*. ∀R:*. λf:(α → R → R). λr:R. r
null =  TmUAbstraction 0 KType $
          TmUAbstraction 1 KType $
            TmAbstraction 0
              (TOperator (TVariable 0 KType) $
                TOperator (TVariable 1 KType) (TVariable 1 KType))
              (TmAbstraction 1 (TVariable 1 KType) $
                TmVariable 1)

-- cons : ∀α:*. ∀R:*. α → ((α → R → R) → R → R) → ((α → R → R) → R → R)
-- cons = Λα:*. ∀R:*. λhead:α. λtail:(α → R → R) → R → R. λf:(α → R → R). λr:R. f head (tail f r)
cons =  TmUAbstraction 0 KType $
          TmUAbstraction 1 KType $
            TmAbstraction 0 (TVariable 0 KType) $
              TmAbstraction 1
                (TOperator
                  (TOperator (TVariable 0 KType) $
                    TOperator (TVariable 1 KType) (TVariable 1 KType))
                  (TOperator (TVariable 1 KType) (TVariable 1 KType)))
                (TmAbstraction 2
                  (TOperator (TVariable 0 KType) $
                    TOperator (TVariable 1 KType) (TVariable 1 KType))
                  (TmAbstraction 3 (TVariable 1 KType) $
                    TmApplication (TmApplication (TmVariable 2) (TmVariable 0)) $
                      TmApplication (TmApplication (TmVariable 1) (TmVariable 2)) (TmVariable 3)))
