{-# LANGUAGE DeriveDataTypeable #-}

module Eval
 ( free
 , occurs
 , subst
 , eval
 , Var (..)
 , Exp (..)
 ) where
  
import Data.List ((\\), union)
import Data.Typeable
import Data.Generics

data Var
	= V String
  | AV String -- Antiquoted variable $var:ident
  deriving (Show, Eq, Typeable, Data)

data Exp
	= Var Var
	| Lam Var Exp
	| App Exp Exp
  | AE String -- Antiquoted expression $exp:ident
  deriving (Show, Eq, Typeable, Data)
	
allBinders :: [Var]
allBinders  = [V [x]        | x <- ['a'..'z']] ++
              [V (x:show i) | x <- ['a'..'z'],
                              i <- [1::Integer ..]]

free :: Exp -> [Var]
free (Var v)   = [v]
free (Lam v e) = free e \\ [v]
free (App e f) = free e `union` free f

occurs :: Exp -> [Var]
occurs (Var v)   = [v]
occurs (Lam v e) = v : occurs e
occurs (App e f) = occurs e `union` occurs f

subst :: Exp -> Var -> Exp -> Exp
subst e x y = subst' (allBinders \\ occurs e `union` occurs y) e x y
  where subst' :: [Var] -> Exp -> Var -> Exp -> Exp
        subst' _ e@(Var v) x y
          | v == x    = y
          | otherwise = e
        subst' fresh e@(Lam v b) x y
          | v == x          = e
          | v `elem` free y = Lam v' (subst' fresh' b' x y)
          | otherwise       = Lam v (subst' fresh b x y)
          where v'     :: Var
                fresh' :: [Var]
                b'     :: Exp
                (v' : fresh') = fresh
                b'            = subst' (error "fresh variables not so fresh") b v (Var v')
        subst' fresh (App e f) x y =
          let e' = subst' fresh e x y
              f' = subst' fresh f x y
          in App e' f'

eval :: Exp -> Exp
eval e@(Var _)   = e
eval e@(Lam _ _) = e
eval (App e f)   =
  case eval e of
    Lam v body -> eval (subst body v f)
    f'         -> App f' (eval f)

