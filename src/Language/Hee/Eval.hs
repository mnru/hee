{-# LANGUAGE OverloadedStrings #-}

module Language.Hee.Eval
  ( eval
  , Value(..)
  ) where

import Data.Text (Text)
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
  deriving (Eq, Show)

type Stack
  = [Value]

eval :: Expression -> Stack -> Result
eval EEmpty                        s = Success EEmpty s
eval (ECompose x y)                s = case eval x s of
                                         Success EEmpty s' -> Success y s'
                                         Success x      s' -> Success (ECompose x y) s'
                                         failure           -> failure
eval (EQuote x)                    s = Success EEmpty  ((VQuote x):s)
eval (ELiteral (LChar x))          s = Success EEmpty   ((VChar x):s )
eval (ELiteral (LString x))        s = Success EEmpty ((VString x):s )
eval (ELiteral (LNumber _ x))      s = Success EEmpty ((VNumber x):s )
eval (EName "id")                  s = Success EEmpty s
eval (EName "pop")            (x:xs) = Success EEmpty xs
eval (EName "dup")            (x:xs) = Success EEmpty (x:x:xs)
eval (EName "dig")        (w:x:y:zs) = Success EEmpty (y:w:x:zs)
eval (EName "swap")         (x:y:zs) = Success EEmpty (y:x:zs)
eval (EName "quote")          (x:xs) = Success EEmpty (VQuote (quote x):xs)
eval (EName "apply") ((VQuote e):xs) = Success e xs
eval (EName "dip") ((VQuote e):x:ys) = case eval e ys of
                                         Success e ys -> Success e (x:ys)
                                         failure      -> failure
eval (EName "+") ((VNumber x):(VNumber y):zs) = Success EEmpty ((VNumber $ x + y):zs)
eval (EName "-") ((VNumber x):(VNumber y):zs) = Success EEmpty ((VNumber $ x - y):zs)
eval (EName "*") ((VNumber x):(VNumber y):zs) = Success EEmpty ((VNumber $ x * y):zs)
eval (EName "/") ((VNumber x):(VNumber y):zs) = Success EEmpty ((VNumber $ x `div` y):zs)
eval (EName "%") ((VNumber x):(VNumber y):zs) = Success EEmpty ((VNumber $ x `mod` y):zs)
eval e s                                      = Failure e s

quote :: Value -> Expression
quote (VChar   x) = ELiteral (LChar x)
quote (VString x) = ELiteral (LString x)
quote (VNumber x) = ELiteral (LNumber Decimal x)
quote (VQuote  x) = EQuote x
