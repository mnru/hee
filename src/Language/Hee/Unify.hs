module Language.Hee.Unify
  (
  ) where

import Data.List ((\\))

import Language.Hee.Syntax

class Kinded a where
  kind :: a -> Kind

instance Kinded Kind where
  kind = id

instance Kinded Type where
  kind (TConstructor _ k) = k
  kind (TVariable x)      = kind x
  kind (TForall x _ t)    = kind t
  kind (TQualified _ t)   = kind t
  kind (TStack _)         = KStack
  kind (TApplication t u) = let (KConstructor _ k) = kind t in k

instance Kinded Stack where
  kind = const KStack

instance Kinded Variable where
  kind (Variable _ k) = k

data UnifyError
  = OccursCheck
  | KindMismatch
  | TypeMismatch
  deriving (Eq, Show)

type Substitution
  = [(Variable, Type)]

class Substitute a where
  freevars   :: a -> [Variable]
  substitute :: Substitution -> a -> a

instance Substitute Stack where
  freevars SEmpty       = []
  freevars (STail id)   = [Variable id KStack]
  freevars (SPush h t)  = freevars h ++ freevars t

  substitute s SEmpty      = SEmpty
  substitute s (SPush h t) = SPush (substitute s h) (substitute s t)
  substitute s (STail id)  = case lookup (Variable id KStack) s of
                               Just (TStack t) -> t
                               _               -> STail id

instance Substitute Type where
  freevars (TConstructor t _)   = [] 
  freevars (TStack s)           = freevars s
  freevars (TApplication t t')  = freevars t ++ freevars t'
  freevars (TForall x _ t)      = freevars t \\ [x]
  freevars (TQualified _ t)     = freevars t
  freevars (TVariable v)        = [v]

  substitute s (TStack t)           = TStack $ substitute s t
  substitute s (TApplication t t')  = TApplication (substitute s t) (substitute s t')
  substitute s (TForall x b t)      = undefined
  substitute s (TQualified ps t)    = undefined
  substitute s (TVariable v)        = maybe (TVariable v) id $ lookup v s
  substitute s t                    = t

class Unify a where
  unify   :: a -> a -> Either UnifyError Substitution
  match   :: a -> a -> Either UnifyError Substitution
  bindvar :: Variable -> a -> Either UnifyError Substitution

instance Unify Stack where
  unify SEmpty SEmpty             = return []
  unify (STail id) s              = undefined 
  unify s (STail id)              = undefined
  unify (SPush h t) (SPush h' t') = undefined
  unify _ _                       = Left TypeMismatch


  match SEmpty SEmpty             = return []
  match (STail id) s              = bindvar (Variable id KStack) s
  match (SPush h t) (SPush h' t') = do a <- match h h'
                                       b <- match (substitute a t) (substitute a t')
                                       return undefined
                                       -- merge a b
  match _ _                       = Left TypeMismatch


  bindvar v@(Variable id k) t
    | v `elem` freevars t = Left OccursCheck
    | k /= kind t         = Left KindMismatch
    | t == (STail id)     = return []
    | otherwise           = return [(v, TStack t)]


instance Unify Type where
  unify (TVariable v) t                       = bindvar v t
  unify t (TVariable v)                       = bindvar v t
  unify (TStack s) (TStack s')                = unify s s'
  unify (TQualified ps t) (TQualified ps' t') = return undefined
  unify (TForall v b t) (TForall v' b' t')    = return undefined
  unify (TConstructor id k) (TConstructor id' k')
    | id == id' && k == k'                    = return []
    | id == id'                               = Left KindMismatch
    | otherwise                               = Left TypeMismatch
  unify (TApplication i o) (TApplication i' o')
                                              = do a <- unify i i'
                                                   b <- unify (substitute a o) (substitute a o')
                                                   return undefined
                                                   --return (a @@ b)
  unify _ _                                   = Left TypeMismatch


  match (TVariable v) t                       = bindvar v t
  match (TStack s) (TStack s')                = match s s'
  match (TQualified ps t) (TQualified ps' t') = undefined
  match (TForall v b t) (TForall v' b' t')    = undefined
  match (TConstructor id k) (TConstructor id' k')
    | id == id' && k == k'                    = return []
    | id == id                                = Left KindMismatch
    | otherwise                               = Left TypeMismatch
  match (TApplication i o) (TApplication i' o')
                                              = do a <- match i i'
                                                   b <- match (substitute a o) (substitute a o')
                                                   return undefined
                                                   --merge a b
  match _ _                                   = Left TypeMismatch


  bindvar v@(Variable id k) t
    | v `elem` freevars t = Left OccursCheck
    | k /= kind t         = Left KindMismatch
    | t == (TVariable v)  = return []
    | otherwise           = return [(v,t)]
  
