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
  | VNumber Int
  | VQuote Expression
  deriving (Eq)

instance Show Value where
  show (VChar v)    = show v
  show (VString v)  = show v
  show (VNumber v)  = show v
  show (VQuote v)   = renderString (EQuote v)

type Stack
  = [Value]

eval :: Expression -> Stack -> Result
eval EEmpty                   s = Success EEmpty s
eval (ECompose x y)           s = case eval x s of
                                    Success EEmpty s' -> eval y s'
                                    Success x'     s' -> eval (ECompose x' y) s'
                                    failure           -> failure
eval (EQuote x)               s = Success EEmpty  (VQuote x:s)
eval (ELiteral (LChar x))     s = Success EEmpty   (VChar x:s)
eval (ELiteral (LString x))   s = Success EEmpty (VString x:s)
eval (ELiteral (LNumber _ x)) s = Success EEmpty (VNumber x:s)
eval (EName "id")             s = heeId s
eval (EName "pop")            s = heePop s
eval (EName "dup")            s = heeDup s
eval (EName "dig")            s = heeDig s
eval (EName "swap")           s = heeSwap s
eval (EName "quote")          s = heeQuote s
eval (EName "compose")        s = heeCompose s
eval (EName "apply")          s = heeApply s
eval (EName "dip")            s = heeDip s
eval (EName "+")              s = heeAdd s
eval (EName "-")              s = heeSub s
eval (EName "*")              s = heeMul s
eval (EName "/")              s = heeDiv s
eval (EName "%")              s = heeMod s
eval e s                        = Failure e s

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
    quote (VChar   c) = ELiteral (LChar c)
    quote (VString c) = ELiteral (LString c)
    quote (VNumber c) = ELiteral (LNumber Decimal c)
    quote (VQuote  c) = EQuote c
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

heeAdd :: Stack -> Result
heeAdd (VNumber x:VNumber y:zs) = Success EEmpty ((VNumber $ x + y):zs)
heeAdd s                        = Failure (EName "+") s

heeSub :: Stack -> Result
heeSub (VNumber x:VNumber y:zs) = Success EEmpty ((VNumber $ x - y):zs)
heeSub s                        = Failure (EName "-") s

heeMul :: Stack -> Result
heeMul (VNumber x:VNumber y:zs) = Success EEmpty ((VNumber $ x * y):zs)
heeMul s                        = Failure (EName "*") s

heeDiv :: Stack -> Result
heeDiv (VNumber x:VNumber y:zs) = Success EEmpty ((VNumber $ x `div` y):zs)
heeDiv s                        = Failure (EName "/") s

heeMod :: Stack -> Result
heeMod (VNumber x:VNumber y:zs) = Success EEmpty ((VNumber $ x `mod` y):zs)
heeMod s                            = Failure (EName "%") s
