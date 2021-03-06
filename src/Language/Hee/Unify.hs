module Language.Hee.Unify
  ( Unify(..)
  , UnifyError(..)
  ) where

import Prelude hiding (id)
import Language.Hee.Syntax
import Language.Hee.Substitute

data UnifyError
  = OccursCheck
  | KindMismatch
  | TypeMismatch
  deriving (Eq, Show)

class Unify a where
  unify   :: a -> a -> Either UnifyError Substitution
  match   :: a -> a -> Either UnifyError Substitution
  bindvar :: Variable -> a -> Either UnifyError Substitution

instance Unify Stack where
  unify SEmpty SEmpty             = return empty
  unify (STail id) s              = bindvar (Variable id KStack) s
  unify s (STail id)              = bindvar (Variable id KStack) s
  unify (SPush h t) (SPush h' t') = do a <- unify h h'
                                       b <- unify (substitute a t) (substitute a t')
                                       return (a <+ b)
  unify _ _                       = Left TypeMismatch

  match SEmpty SEmpty             = return empty
  match (STail id) s              = bindvar (Variable id KStack) s
  match (SPush h t) (SPush h' t') = do a <- match h h'
                                       b <- match (substitute a t) (substitute a t')
                                       a <+> b
  match _ _                       = Left TypeMismatch

  bindvar v@(Variable id k) t
    | t == STail id       = return empty
    | v `elem` freevars t = Left OccursCheck
    | k /= kind t         = Left KindMismatch
    | otherwise           = return (v +-> TStack t)

instance Unify Type where
  unify (TVariable v) t                       = bindvar v t
  unify t (TVariable v)                       = bindvar v t
  unify (TStack s) (TStack s')                = unify s s'
  unify (TQualified {}) (TQualified {})       = return undefined
  unify (TForall {}) (TForall {})             = return undefined
  unify (TConstructor id k) (TConstructor id' k')
    | id == id' && k == k'                    = return empty
    | id == id'                               = Left KindMismatch
    | otherwise                               = Left TypeMismatch
  unify (TApplication i o) (TApplication i' o')
                                              = do a <- unify i i'
                                                   b <- unify (substitute a o) (substitute a o')
                                                   return (a <+ b)
  unify _ _                                   = Left TypeMismatch

  match (TVariable v) t                       = bindvar v t
  match (TStack s) (TStack s')                = match s s'
  match (TQualified {}) (TQualified {})       = undefined
  match (TForall {}) (TForall {})             = undefined
  match (TConstructor id k) (TConstructor id' k')
    | id == id' && k == k'                    = return empty
    | id == id                                = Left KindMismatch
    | otherwise                               = Left TypeMismatch
  match (TApplication i o) (TApplication i' o')
                                              = do a <- match i i'
                                                   b <- match (substitute a o) (substitute a o')
                                                   a <+> b
  match _ _                                   = Left TypeMismatch

  bindvar v@(Variable _ k) t
    | t == TVariable v    = return empty
    | v `elem` freevars t = Left OccursCheck
    | k /= kind t         = Left KindMismatch
    | otherwise           = return (v +-> t)
