{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module Hee.SystemFw
  ( Term(..)
  , Type(..)
  , Kind(..)
  , EUnify(..)
  , CanSubstitute
  , CanUnify
  , id
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
  , normalize
  , readKind
  , readType
  , readTerm
  , emptyEnv
  , valueOf
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Attoparsec.Text

import Data.List (union, (\\), foldl', elemIndex)
import Control.Applicative (pure, (<|>), (<*), (*>), (<$>))
import Prelude hiding (id, succ, fst, snd, sum, null, takeWhile, readParen)

-- Data Types
---------------------------------------------------------------------------

type Id = Int

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

-- Kindable
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
  kind k = k

-- Normalization
---------------------------------------------------------------------------

-- Can re-order directly nested quantifiers
--   (∀α:★. (∀β:★. τ)) ≡ (∀β:★. (∀α:★. τ))
--
-- Can't re-order indirectly nested quantifiers
--   (∀α:★. υ (∀β:★. τ)) ≢  (∀β:★. υ (∀α:★. τ))
--
-- Can lift quantifiers on the right of an arrow
--   (∀α:★. τ → (∀β:★. υ)) ≡ (∀α:★. (∀β:★. τ → υ))
--
-- Can't lift quantifiers on the left of an arrow
--   (∀α:★. (∀β:★. τ) → τ) ≢ (∀α:★. (∀β:★. τ → τ))

normalize (TVariable a k)         = TVariable a k
normalize (TApplication t u)      = TApplication (normalize t) (normalize u)
normalize (TAbstraction a k t)    = TAbstraction a k (normalize t)
normalize (TOperator t u)         =
  case normalize u of
    (TQuantification b k u') -> normalize (TQuantification b k (TOperator (normalize t) u'))
    u'                       -> TOperator (normalize t) u'
normalize (TQuantification a k t) =
  case normalize t of
    (TQuantification b k' t') ->
      if b < a
      then normalize (TQuantification b k' (TQuantification a k  t'))
      else TQuantification a k  (TQuantification b k' t')
    t' -> TQuantification a k t'

-- Substitution
---------------------------------------------------------------------------

type Variable       = (Id, Kind)
type Substitution a = [(Variable, a)]

unbindvar :: Variable -> Substitution a -> Substitution a
unbindvar v s = filter (\(w, _) -> w /= v) s

class CanSubstitute t where
  substitute :: Substitution Type -> t -> t
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

instance CanSubstitute Term where
  substitute s (TmVariable x)         = TmVariable x
  substitute s (TmApplication f e)    = TmApplication (substitute s f) (substitute s e)
  substitute s (TmAbstraction x t e)  = TmAbstraction x (substitute s t) (substitute s e)
  substitute s (TmUAbstraction a k e) = TmUAbstraction a k (substitute (unbindvar (a, k) s) e)
  substitute s (TmUApplication f t)   = TmUApplication (substitute s f) (substitute s t)
  
  freevars (TmVariable x)             = []
  freevars (TmApplication f e)        = freevars f `union` freevars e
  freevars (TmAbstraction x t e)      = freevars t `union` freevars e
  freevars (TmUAbstraction a k e)     = freevars e \\ [(a, k)]
  freevars (TmUApplication f t)       = freevars f `union` freevars t

-- Unification
---------------------------------------------------------------------------

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
  unify (TApplication t u) (TApplication t' u') = undefined
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

-- Pretty Printing
---------------------------------------------------------------------------

showTmId x = showId x "abcdefghijklmnopqrstuvwxyz"
showTId a  = showId a "αβγδεζηθικλμνξοπρςστυφχψω"

showId id alphabet = (alphabet !! n) : (replicate k '\'')
  where
    k = id `div` length alphabet
    n = id `mod` length alphabet

showTerm :: Int -> Term -> String
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

showType :: Int -> Type -> String
showType 0 (TVariable a _)         = showTId a
showType 0 (TOperator t u)         = showType 1 t ++ " → " ++ showType 1 u
showType 0 (TApplication a b)      = showType 1 a ++ " "   ++ showType 1 b
showType 0 (TAbstraction a k t)    = "λ" ++ showTId a ++ ":" ++ showKind 0 k ++ ". " ++ showType 0 t
showType 0 (TQuantification a k t) = "∀" ++ showTId a ++ ":" ++ showKind 0 k ++ ". " ++ showType 0 t
showType n (TVariable a _)         = showTId a
showType n (TOperator t u)         = "(" ++ showType (n+1) t ++ " → " ++ showType (n+1) u ++ ")"
showType n (TApplication a b)      = "(" ++ showType (n+1) a ++ " "   ++ showType (n+1) b ++ ")"
showType n (TAbstraction a k t)    = "(λ" ++ showTId a ++ ":" ++ showKind (n+1) k ++ ". " ++ showType 0 t ++ ")"
showType n (TQuantification a k t) = "(∀" ++ showTId a ++ ":" ++ showKind (n+1) k ++ ". " ++ showType 0 t ++ ")"

showKind :: Int -> Kind -> String
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

-- Parser
---------------------------------------------------------------------------

readArrow, readForall, readLambda, readLambda :: Parser ()
readArrow  = pure () <* (string "→" <|> string "->")
readForall = pure () <* (string "∀" <|> string "forall ")
readLambda = pure () <* (string "λ" <|> string ",\\" <|> string "lambda ")
readLAMBDA = pure () <* (string "Λ" <|> string "/\\" <|> string "LAMBDA ")

readParen :: Parser a -> Parser a
readParen inside =
  do _ <- char '('
     e <- inside
     _ <- skipSpace
     _ <- char ')'
     return e

-- Tracks closed type variables [(Id, Kind)] so we can resolve each
-- occurance of a variable with its kind. 
type TVariableScope = [Variable]

-- Reads the Id of a type variable
readTId :: TVariableScope -> Parser Id
readTId s =
  do a <- satisfy (inClass alphabet)
     n <- countiks
     let (Just m) = elemIndex a alphabet
     return $ m + n * length alphabet
  where
    alphabet = "αβγδεζηθικλμνξοπρςστυφχψω"
    countiks = T.length `fmap` takeWhile (== '\'')

readKind :: Parser Kind
readKind = skipSpace *> (readParen readKind <|> readKType) >>= readKind'
  where
    -- Read the "rest" of an expression
    readKind' :: Kind -> Parser Kind
    readKind' k = readKOperator k <|> pure k

    readKType :: Parser Kind
    readKType = (char '★' <|> char '*') *> pure KType

    readKOperator :: Kind -> Parser Kind
    readKOperator k =
      do _ <- skipSpace
         _ <- string "→" <|> string "->"
         _ <- skipSpace
         l <- readKind
         return $ KOperator k l

readType :: TVariableScope -> Parser Type
readType s =
  do _ <- skipSpace
     t <- readParen (readType s)
      <|> readTQuantification s
      <|> readTAbstraction s
      <|> readTVariable s
     readType' s t
  where
    -- Read the "rest" of an expression
    readType' :: TVariableScope -> Type -> Parser Type
    readType' s t = readTOperator s t
                <|> readTApplication s t
                <|> pure t

    -- α,β
    readTVariable :: TVariableScope -> Parser Type
    readTVariable s =
      do a <- satisfy (inClass alphabet)
         n <- countiks
         let (Just m) = elemIndex a alphabet
         let id       = m + n * length alphabet
         return $ TVariable id (lookupKind s id)
      where
        alphabet = "αβγδεζηθικλμνξοπρςστυφχψω"
        countiks = T.length `fmap` takeWhile (== '\'')

        -- TODO: strict, error evaluation
        lookupKind :: TVariableScope -> Id -> Kind
        lookupKind ((b, k):vs) a
          | b == a      = k
          | otherwise   = lookupKind vs a
        lookupKind [] a = error $ "type variable " ++ showTId a ++ " is not bound"

    -- τ υ
    readTApplication :: TVariableScope -> Type -> Parser Type
    readTApplication s t = TApplication t <$> readType s

    -- λα:κ. τ
    readTAbstraction :: TVariableScope -> Parser Type
    readTAbstraction s =
      do readLambda
         a <- readTId s <* char ':'
         k <- readKind  <* char '.'
         t <- readType ((a, k):s)
         return $ TAbstraction a k t

    -- ∀α:κ. τ
    readTQuantification :: TVariableScope -> Parser Type
    readTQuantification s =
      do readForall
         a <- readTId s <* char ':'
         k <- readKind  <* char '.'
         t <- readType ((a, k):s)
         return $ TQuantification a k t

    -- τ → υ
    readTOperator :: TVariableScope -> Type -> Parser Type
    readTOperator s t =
      do _ <- skipSpace
         _ <- readArrow
         u <- readType s
         return $ TOperator t u

readTerm :: TVariableScope -> Parser Term
readTerm s =
  do _ <- skipSpace
     e <- readParen (readTerm s)
      <|> readTmAbstraction s
      <|> readTmUAbstraction s
      <|> readTmVariable
     readTerm' s e
  where
    -- Read the "rest" of an expression
    readTerm' :: TVariableScope -> Term -> Parser Term
    readTerm' s e = readTmApplication s e
                <|> readTmUApplication s e
                <|> pure e

    readTmId :: Parser Id
    readTmId =
      do x <- satisfy (inClass alphabet)
         n <- countiks
         let (Just m) = elemIndex x alphabet
         return $ m + n * (length alphabet)
      where
        alphabet = "abcdefghijklmnopqrstuvwxyz"
        countiks = T.length `fmap` takeWhile (== '\'')

    -- x,y
    readTmVariable :: Parser Term
    readTmVariable = TmVariable <$> readTmId

    -- f e
    readTmApplication :: TVariableScope -> Term -> Parser Term
    readTmApplication s f = TmApplication f <$> readTerm s

    -- λx:τ. e
    readTmAbstraction :: TVariableScope -> Parser Term
    readTmAbstraction s =
      do readLambda
         x <- readTmId   <* char ':'
         t <- readType s <* char '.'
         e <- readTerm s
         return $ TmAbstraction x t e

    -- Λα:κ. e
    readTmUAbstraction :: TVariableScope -> Parser Term
    readTmUAbstraction s =
      do readLAMBDA
         a <- readTId s <* char ':'
         k <- readKind  <* char '.'
         e <- readTerm ((a, k):s)
         return $ TmUAbstraction a k e

    -- f τ
    readTmUApplication :: TVariableScope -> Term -> Parser Term
    readTmUApplication s f = TmUApplication f <$> readType s

instance Read Term where
  readsPrec _ s =
    case result of
      Done xs x -> [(x, T.unpack xs)]
      _         -> []
    where
      parser = readTerm []
      result = feed (parse parser $ T.pack s) T.empty

instance Read Type where
  readsPrec _ s =
    case result of
      Done xs x -> [(x, T.unpack xs)]
      _         -> []
    where
      parser = readType []
      result = feed (parse parser $ T.pack s) T.empty

instance Read Kind where
  readsPrec _ s =
    case result of
      Done xs x -> [(x, T.unpack xs)]
      _         -> []
    where
      parser = readKind
      result = feed (parse parser $ T.pack s) T.empty

-- Evaluation
---------------------------------------------------------------------------

type Environment a = [(Id, a)]

data EValueOf
  = EUnbound Id
  | EStuck Term
  deriving (Eq, Show)

-- Create an empty environment
emptyEnv :: Environment a
emptyEnv = []

-- Add a binding to the environment
extendEnv :: Environment a -> (Id, a) -> Environment a
extendEnv env v = v:env

-- Remove a binding from the environment
reduceEnv :: Environment a -> Id -> Environment a
reduceEnv [] x = []
reduceEnv ((x', t):env) x
  | x' == x    = reduceEnv env x
  | otherwise  = (x', t):reduceEnv env x

-- Resolve an identifier in the environment
searchEnv :: Environment a -> Id -> Either EValueOf a
searchEnv [] x            = Left (EUnbound x)
searchEnv ((x', t):env) x = if x /= x'
                            then searchEnv env x
                            else Right t

valueOf :: Environment Term -> Term -> Either EValueOf Term
valueOf env t = valueOf' env searchEnv t
  where 
    safeResolver env x
      = case searchEnv env x of
          Left _  -> Right $ TmVariable x
          Right t -> Right t

    valueOf' env resolver (TmApplication f e)
      = case valueOf' env resolver f of
          (Right (TmAbstraction x _ f')) ->
            case valueOf' env resolver e of
              (Right e') -> valueOf' (extendEnv env (x, e')) resolver f'
              x          -> x
          (Right f') -> Right f'
          x          -> x
    valueOf' env resolver e@(TmUApplication f t)
      = case valueOf' env resolver f of
          (Right (TmUAbstraction a k e)) ->
            case bindvar (a, k) t of
              (Right s) -> valueOf' env resolver $ substitute s e
              _         -> Left $ EStuck $ TmUAbstraction a k e
          (Right f') -> Right f'
          x          -> x
    valueOf' env resolver (TmVariable x)
      = resolver env x
    -- Performing reduction under lambdas
    --
    -- valueOf [] ((λt. (λf. t)) 100)
    -- valueOf [t/100] (λf. t)
    --
    -- While (λf. t) is in normal form, it's not a closed term. Applying it
    -- to an argument would evaluate to the free variable t, though we would
    -- expect the result to be 100.
    --
    -- Either we need to allow evaluation under lambda terms, or we extend
    -- the definition of TmAbstraction to include the environment which
    -- closes over its free variables.
    --
    -- Since our language is strongly normalizing, we can safely do reduction
    -- under lambdas without being concerned about non-termination
    --
    valueOf' env _ (TmAbstraction x t e)
      = case (valueOf' (reduceEnv env x) safeResolver e) of
          (Right e') -> Right (TmAbstraction x t e')
          x          -> x
    valueOf' env _ (TmUAbstraction a k e)
      = case (valueOf' env safeResolver e) of
          (Right e') -> Right (TmUAbstraction a k e')
          x          -> x

-- Examples
---------------------------------------------------------------------------

-- id : ∀α:★. α → α
-- id = Λα:★. λa:α. a
id  = TmUAbstraction 0 KType $
        TmAbstraction 0 (TVariable 0 KType) $
          TmVariable 0

-- Bool : ★
-- Bool = ∀α:★. α → α → α
bool  = TQuantification 0 KType $
          TOperator (TVariable 0 KType) $
            TOperator (TVariable 0 KType) (TVariable 0 KType)

-- tru : ∀α:★. α → α → α
-- tru = Λα:★. λt:α. λf:α. t
tru = TmUAbstraction 0 KType $
         TmAbstraction 19 (TVariable 0 KType) $
           TmAbstraction 5 (TVariable 0 KType) $
             TmVariable 19

-- fls : ∀α:★. α → α → α
-- fls = Λα:★. λt:α. λf:α. f
fls = TmUAbstraction 0 KType $
        TmAbstraction 19 (TVariable 0 KType) $
          TmAbstraction 5 (TVariable 0 KType) $
            TmVariable 5

-- Nat : ★
-- Nat = ∀α:★. α → (α → α) → α
nat = TQuantification 0 KType $
        TOperator (TVariable 0 KType) $
          TOperator
            (TOperator (TVariable 0 KType) (TVariable 0 KType))
            (TVariable 0 KType)

-- zero : ∀α:★. α → (α → α) → α
-- zero = Λα:★. λa:α. λf:α → α. a
zero =  TmUAbstraction 0 KType $
          TmAbstraction 0 (TVariable 0 KType) $
            TmAbstraction 5 (TOperator (TVariable 0 KType) (TVariable 0 KType)) $
              TmVariable 0

-- succ : ∀α:★. α → (α → α) → α
-- succ = Λα:★. λa:α. λf:α → α. f a
succ =  TmUAbstraction 0 KType $
          TmAbstraction 0 (TVariable 0 KType) $
            TmAbstraction 5 (TOperator (TVariable 0 KType) (TVariable 0 KType)) $
              TmApplication (TmVariable 5) (TmVariable 0)

-- Product : ★ → (★ → ★)
-- Product = λα:★. λβ:★. ∀ρ:★. (α → β → ρ) → ρ
pair =  TAbstraction 0 KType $
          TAbstraction 1 KType $
            TQuantification 15 KType $
              TOperator
                (TOperator (TVariable 0 KType) $
                  TOperator (TVariable 1 KType) (TVariable 15 KType))
                (TVariable 15 KType)

-- fst : ∀α:★. ∀β:★. ∀ρ:★. (α → β → ρ) → α
-- fst = Λα:★. Λβ:★. λp:(∀ρ:★. α → β → ρ). p α (λa:α. λb:β. a)
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

-- snd : ∀α:★. ∀β:★. ∀ρ:★. (α → β → ρ) → β 
-- snd = Λα:★. Λβ:★. λp:(∀ρ:★. α → β → ρ). p β (λa:α. λb:β. b)
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
-- Sum = λα:★. λβ:★. ∀ρ:★. (α → ρ) → (β → ρ) → ρ
sum = TAbstraction 0 KType $
        TAbstraction 1 KType $
          TQuantification 2 KType $
            TOperator (TOperator (TVariable 0 KType) (TVariable 2 KType)) $
              TOperator (TOperator (TVariable 1 KType) (TVariable 2 KType)) $
                TVariable 2 KType

-- inL : ∀α:★. ∀β:★. α → (∀ρ:★. (α → ρ) → (β → ρ) → ρ) → α
-- inL = Λα:★. Λβ:★. ∀ρ:★. λa:α. λleft:(α → ρ). λright:(β → ρ). left a
inl = TmUAbstraction 0 KType $
        TmUAbstraction 1 KType $
          TmUAbstraction 2 KType $
            TmAbstraction 0 (TVariable 0 KType) $
              TmAbstraction 1 (TOperator (TVariable 0 KType) (TVariable 2 KType)) $
                TmAbstraction 2 (TOperator (TVariable 1 KType) (TVariable 2 KType)) $
                  TmApplication (TmVariable 1) (TmVariable 0)

-- inL : ∀α:★. ∀β:★. α → (∀ρ:★. (α → ρ) → (β → ρ) → ρ) → β
-- inR = Λα:★. Λβ:★. ∀ρ:★. λv:β. λleft:(α → ρ). λright:(β → ρ). right a
inr = TmUAbstraction 0 KType $
        TmUAbstraction 1 KType $
          TmUAbstraction 2 KType $
            TmAbstraction 0 (TVariable 1 KType) $
              TmAbstraction 1 (TOperator (TVariable 0 KType) (TVariable 2 KType)) $
                TmAbstraction 2 (TOperator (TVariable 1 KType) (TVariable 2 KType)) $
                  TmApplication (TmVariable 2) (TmVariable 0)

-- List : ★ → ★
-- List = λα:★. ∀ρ:★. (α → ρ → ρ) → ρ → ρ
list =  TAbstraction 0 KType $
          TQuantification 1 KType $
            TOperator
              (TOperator
                (TVariable 0 KType)
                (TOperator (TVariable 1 KType) (TVariable 1 KType)))
              (TOperator (TVariable 1 KType) (TVariable 1 KType))

-- null : ∀α:★. ∀ρ:★. (α → ρ → ρ) → ρ → ρ
-- null = Λα:★. ∀ρ:★. λf:(α → ρ → ρ). λr:ρ. r
null =  TmUAbstraction 0 KType $
          TmUAbstraction 1 KType $
            TmAbstraction 0
              (TOperator (TVariable 0 KType) $
                TOperator (TVariable 1 KType) (TVariable 1 KType))
              (TmAbstraction 1 (TVariable 1 KType) $
                TmVariable 1)

-- cons : ∀α:★. ∀ρ:★. α → ((α → ρ → ρ) → ρ → ρ) → ((α → ρ → ρ) → ρ → ρ)
-- cons = Λα:★. ∀ρ:★. λh:α. λt:(α → ρ → ρ) → ρ → ρ. λf:(α → ρ → ρ). λr:ρ. f h (t f r)
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
