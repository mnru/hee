{-# LANGUAGE OverloadedStrings #-}

module Language.Hee.Eval
  ( eval
  , Value(..)
  , Result(..)
  ) where

import Data.Text (Text)
import Language.Hee.Pretty (renderString)
import Language.Hee.Syntax hiding (Stack)

data Result
  = Success Expression Stack
  | Failure Expression Stack
  deriving (Eq, Show)

data Value
  = VChar Char
  | VString Text
  | VInteger Int
  | VFloat Float
  | VQuote Expression
  | VBool Bool
  deriving (Eq)

instance Show Value where
  show (VChar v)    = renderString (LChar v)
  show (VString v)  = renderString (LString v)
  show (VInteger v) = renderString (LInteger Decimal v)
  show (VFloat v)   = renderString (LFloat v)
  show (VQuote v)   = renderString (EQuote v)
  show (VBool v)    = renderString (LBool v)

type Stack
  = [Value]

eval :: Expression -> Stack -> Result
eval EEmpty                    s = Success EEmpty s
eval (ECompose x y)            s = case eval x s of
                                     Success EEmpty s' -> eval y s'
                                     Success x'     s' -> eval (ECompose x' y) s'
                                     failure           -> failure
eval (EQuote x)                s = Success EEmpty   (VQuote x:s)
eval (ELiteral (LChar x))      s = Success EEmpty    (VChar x:s)
eval (ELiteral (LString x))    s = Success EEmpty  (VString x:s)
eval (ELiteral (LInteger _ x)) s = Success EEmpty (VInteger x:s)
eval (ELiteral (LFloat x))     s = Success EEmpty   (VFloat x:s)
eval (ELiteral (LBool x))      s = Success EEmpty    (VBool x:s)
eval (EName "id")              s = heeId s
eval (EName "pop")             s = heePop s
eval (EName "dup")             s = heeDup s
eval (EName "dig")             s = heeDig s
eval (EName "swap")            s = heeSwap s
eval (EName "quote")           s = heeQuote s
eval (EName "compose")         s = heeCompose s
eval (EName "apply")           s = heeApply s
eval (EName "dip")             s = heeDip s
eval (EName "u")               s = heeU s
eval (EName "both")            s = heeBoth s
eval (EName "+")               s = heeAdd s
eval (EName "-")               s = heeSub s
eval (EName "*")               s = heeMul s
eval (EName "/")               s = heeDiv s
eval (EName "%")               s = heeMod s
eval (EName "if")              s = heeIf s
eval (EName "or")              s = heeOr s
eval (EName "and")             s = heeAnd s
eval (EName "not")             s = heeNot s
eval (EName "==")              s = heeEq s
eval (EName "/=")              s = heeNe s
eval (EName "<")               s = heeLt s
eval (EName ">")               s = heeGt s
eval (EName "<=")              s = heeLte s
eval (EName ">=")              s = heeGte s
eval e s                         = Failure e s

heeId :: Stack -> Result
heeId = Success EEmpty

heePop :: Stack -> Result
heePop (_:xs) = Success EEmpty xs
heePop s      = Failure (EName "pop") s

heeDup :: Stack -> Result
heeDup (x:xs) = Success EEmpty (x:x:xs)
heeDup s      = Failure (EName "dup") s

heeDig :: Stack -> Result
heeDig (w:x:y:zs) = Success EEmpty (y:w:x:zs)
heeDig s          = Failure (EName "dig") s

heeSwap :: Stack -> Result
heeSwap (x:y:zs) = Success EEmpty (y:x:zs)
heeSwap s        = Failure (EName "swap") s

heeQuote :: Stack -> Result
heeQuote (x:xs) = Success EEmpty (VQuote (quote x):xs)
  where
    quote :: Value -> Expression
    quote (VChar   c)  = ELiteral (LChar c)
    quote (VString c)  = ELiteral (LString c)
    quote (VInteger c) = ELiteral (LInteger Decimal c)
    quote (VFloat c)   = ELiteral (LFloat c)
    quote (VQuote  c)  = EQuote c
    quote (VBool   c)  = ELiteral (LBool c)
heeQuote s      = Failure (EName "quote") s

heeCompose :: Stack -> Result
heeCompose (VQuote g:VQuote f:xs) = Success EEmpty (VQuote (ECompose f g):xs)
heeCompose s                      = Failure (EName "compose") s

heeApply :: Stack -> Result
heeApply (VQuote e:xs) = eval e xs
heeApply s             = Failure (EName "apply") s

heeDip :: Stack -> Result
heeDip (VQuote e:x:ys) = case eval e ys of
                           Success e' ys' -> Success e' (x:ys')
                           failure        -> failure
heeDip s               = Failure (EName "dip") s

heeU :: Stack -> Result
heeU = eval (ECompose (EName "dup") (EName "apply"))

heeAdd :: Stack -> Result
heeAdd (VInteger x:VInteger y:zs) = Success EEmpty ((VInteger $ y + x):zs)
heeAdd (VFloat x:VFloat y:zs)     = Success EEmpty ((VFloat   $ y + x):zs)
heeAdd s                          = Failure (EName "+") s

heeSub :: Stack -> Result
heeSub (VInteger x:VInteger y:zs) = Success EEmpty ((VInteger $ y - x):zs)
heeSub (VFloat x:VFloat y:zs)     = Success EEmpty ((VFloat   $ y - x):zs)
heeSub s                          = Failure (EName "-") s

heeMul :: Stack -> Result
heeMul (VInteger x:VInteger y:zs) = Success EEmpty ((VInteger $ y * x):zs)
heeMul (VFloat x:VFloat y:zs)     = Success EEmpty ((VFloat   $ y * x):zs)
heeMul s                          = Failure (EName "*") s

heeDiv :: Stack -> Result
heeDiv (VInteger x:VInteger y:zs) = Success EEmpty ((VInteger $ y `div` x):zs)
heeDiv (VFloat x:VFloat y:zs)     = Success EEmpty ((VFloat   $     y / x):zs)
heeDiv s                          = Failure (EName "/") s

heeMod :: Stack -> Result
heeMod (VInteger x:VInteger y:zs) = Success EEmpty ((VInteger $ y `mod` x):zs)
heeMod s                        = Failure (EName "%") s

heeIf :: Stack -> Result
heeIf (VQuote _:VQuote t:VBool True :zs) = eval t zs
heeIf (VQuote f:VQuote _:VBool False:zs) = eval f zs
heeIf s                                  = Failure (EName "if") s

heeOr :: Stack -> Result
heeOr (VBool x:VBool y:zs) = Success EEmpty ((VBool $ y || x):zs)
heeOr s                    = Failure (EName "or") s

heeAnd :: Stack -> Result
heeAnd (VBool x:VBool y:zs) = Success EEmpty ((VBool $ y && x):zs)
heeAnd s                    = Failure (EName "&&") s

heeNot :: Stack -> Result
heeNot (VBool x:ys) = Success EEmpty ((VBool $ not x):ys)
heeNot s            = Failure (EName "not") s

heeEq :: Stack -> Result
heeEq (VBool x:VBool y:zs)       = Success EEmpty ((VBool $ y == x):zs)
heeEq (VChar x:VChar y:zs)       = Success EEmpty ((VBool $ y == x):zs)
heeEq (VInteger x:VInteger y:zs) = Success EEmpty ((VBool $ y == x):zs)
heeEq (VFloat x:VFloat y:zs)     = Success EEmpty ((VBool $ y == x):zs)
heeEq (VString x:VString y:zs)   = Success EEmpty ((VBool $ y == x):zs)
heeEq s                          = Failure (EName "==") s

heeNe :: Stack -> Result
heeNe (VBool x:VBool y:zs)       = Success EEmpty ((VBool $ y /= x):zs)
heeNe (VChar x:VChar y:zs)       = Success EEmpty ((VBool $ y /= x):zs)
heeNe (VInteger x:VInteger y:zs) = Success EEmpty ((VBool $ y /= x):zs)
heeNe (VFloat x:VFloat y:zs)     = Success EEmpty ((VBool $ y /= x):zs)
heeNe (VString x:VString y:zs)   = Success EEmpty ((VBool $ y /= x):zs)
heeNe s                          = Failure (EName "/=") s

heeLt :: Stack -> Result
heeLt (VInteger x:VInteger y:zs) = Success EEmpty ((VBool $ y < x):zs)
heeLt (VString x:VString y:zs)   = Success EEmpty ((VBool $ y < x):zs)
heeLt (VFloat x:VFloat y:zs)     = Success EEmpty ((VBool $ y < x):zs)
heeLt (VChar x:VChar y:zs)       = Success EEmpty ((VBool $ y < x):zs)
heeLt s                          = Failure (EName "<") s

heeGt :: Stack -> Result
heeGt (VInteger x:VInteger y:zs) = Success EEmpty ((VBool $ y > x):zs)
heeGt (VString x:VString y:zs)   = Success EEmpty ((VBool $ y > x):zs)
heeGt (VFloat x:VFloat y:zs)     = Success EEmpty ((VBool $ y > x):zs)
heeGt (VChar x:VChar y:zs)       = Success EEmpty ((VBool $ y > x):zs)
heeGt s                          = Failure (EName ">") s

heeLte :: Stack -> Result
heeLte (VInteger x:VInteger y:zs) = Success EEmpty ((VBool $ y <= x):zs)
heeLte (VString x:VString y:zs)   = Success EEmpty ((VBool $ y <= x):zs)
heeLte (VFloat x:VFloat y:zs)     = Success EEmpty ((VBool $ y <= x):zs)
heeLte (VChar x:VChar y:zs)       = Success EEmpty ((VBool $ y <= x):zs)
heeLte s                          = Failure (EName "<=") s

heeGte :: Stack -> Result
heeGte (VInteger x:VInteger y:zs) = Success EEmpty ((VBool $ y >= x):zs)
heeGte (VString x:VString y:zs)   = Success EEmpty ((VBool $ y >= x):zs)
heeGte (VFloat x:VFloat y:zs)     = Success EEmpty ((VBool $ y >= x):zs)
heeGte (VChar x:VChar y:zs)       = Success EEmpty ((VBool $ y >= x):zs)
heeGte s                          = Failure (EName ">=") s

-- dup [swap [apply] dip] dip apply
heeBoth :: Stack -> Result
heeBoth = eval
 (ECompose
   (EName "dup")
   (ECompose
     (EQuote
       (ECompose
         (EName "swap")
         (ECompose
           (EQuote (EName "apply"))
           (EName "dip"))))
     (ECompose (EName "dip") (EName "apply"))))
