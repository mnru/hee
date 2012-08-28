module Language.Hee.Substitute
  ( Substitution
  , Substitute(..)
  , empty
  , (+->)
  , (<+)
  , (<+>)
  ) where

import Prelude hiding (id)
import Data.List (intersect, nub, (\\))
import Data.Monoid
import Data.Maybe

import Language.Hee.Syntax

newtype Substitution
  = S { unS :: [(Variable, Type)] }
  deriving (Show, Eq)

-- Empty substitution
empty :: Substitution
empty = S []

-- Singleton substitution
infixr 4 +->
(+->) :: Variable -> Type -> Substitution
(+->) v t
  | kind v == kind t = S [(v, t)]
  | otherwise        = error "+->: kind mismatch"

-- Composition of substitutions left-biased
--   substitute (a <+ b) = substitute a . substitute b
infixr 4 <+
(<+) :: Substitution -> Substitution -> Substitution
(<+) a (S b) = S $ [(v, substitute a t) | (v, t) <- b] ++ b

-- Composition of substitutions symmetric
--   substitute (a <+> b) = substitute (b <+> a)
infixr 4 <+>
(<+>) :: Monad m => Substitution -> Substitution -> m Substitution
(<+>) a@(S as) b@(S bs)
  | match     = return . S $ as ++ bs
  | otherwise = fail "<+>: substitution domains mismatch"
  where match = all (\v -> substitute a (TVariable v) == substitute b (TVariable v))
                    (map fst as `intersect` map fst bs)

instance Monoid Substitution where
  mempty      = empty
  mappend a b = fromJust (a <+> b)

-- TODO we must rename 'a' to avoid accidental variable capture of 'a'!
--   substitute [b +-> a] (Binder a. c b a)
--   = (Binder a. c a a)        wrong
--   = (Binder x. c a x)        right
class Substitute a where
  freevars   :: a -> [Variable]
  substitute :: Substitution -> a -> a

instance Substitute a => Substitute [a] where
  freevars     = nub . concatMap freevars
  substitute s = map (substitute s)

instance (Substitute a, Substitute b) => Substitute (a, b) where
  freevars (a, b)     = freevars a ++ freevars b
  substitute s (a, b) = (substitute s a, substitute s b)

instance Substitute Stack where
  freevars SEmpty       = []
  freevars (STail id)   = [Variable id KStack]
  freevars (SPush h t)  = freevars h ++ freevars t

  substitute _ SEmpty         = SEmpty
  substitute s (SPush h t)    = SPush (substitute s h) (substitute s t)
  substitute (S s) (STail id) = case lookup (Variable id KStack) s of
                                  Just (TStack t) -> t
                                  _               -> STail id

instance Substitute Type where
  freevars (TConstructor _ _)   = []
  freevars (TStack s)           = freevars s
  freevars (TApplication t t')  = freevars t ++ freevars t'
  freevars (TForall x _ t)      = freevars t \\ [x]
  freevars (TQualified _ t)     = freevars t
  freevars (TVariable v)        = [v]

  substitute s (TStack t)           = TStack $ substitute s t
  substitute s (TApplication t t')  = TApplication (substitute s t) (substitute s t')
  substitute _ (TForall {})         = undefined
  substitute _ (TQualified {})      = undefined
  substitute (S s) (TVariable v)    = fromMaybe (TVariable v) $ lookup v s
  substitute _ t                    = t
