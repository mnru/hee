{-# LANGUAGE OverloadedStrings #-}

module Language.Hee.Eval
  ( eval
  , evalMain
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

type Environment
  = Text -> Maybe Expression

readEnv :: [Declaration] -> Environment
readEnv (DNameBind id' _ _ e:rest) id
  | id == id' = Just e
  | otherwise = readEnv rest id
readEnv [] _  = Nothing

evalMain :: [Declaration] -> Stack -> Result
evalMain ds = eval (readEnv ds) (EName "main")

eval :: Environment -> Expression -> Stack -> Result
eval _   EEmpty                    s = Success EEmpty s
eval env (ECompose x y)            s = case eval env x s of
                                         Success EEmpty s' -> eval env y s'
                                         Success x'     s' -> eval env (ECompose x' y) s'
                                         failure           -> failure
eval _   (EQuote x)                s = Success EEmpty   (VQuote x:s)
eval _   (ELiteral (LChar x))      s = Success EEmpty    (VChar x:s)
eval _   (ELiteral (LString x))    s = Success EEmpty  (VString x:s)
eval _   (ELiteral (LInteger _ x)) s = Success EEmpty (VInteger x:s)
eval _   (ELiteral (LFloat x))     s = Success EEmpty   (VFloat x:s)
eval _   (ELiteral (LBool x))      s = Success EEmpty    (VBool x:s)
eval _   (EName "id")              s = heeId      s
eval _   (EName "pop")             s = heePop     s
eval _   (EName "dup")             s = heeDup     s
eval _   (EName "dup2")            s = heeDup2    s
eval _   (EName "dig")             s = heeDig     s
eval _   (EName "swap")            s = heeSwap    s
eval _   (EName "bury")            s = heeBury    s
eval _   (EName "quote")           s = heeQuote   s
eval _   (EName "compose")         s = heeCompose s
eval env (EName "apply")           s = heeApply   env s
eval env (EName "dip")             s = heeDip     env s
eval env (EName "u")               s = heeU       env s
eval env (EName "both")            s = heeBoth    env s
eval _   (EName "+")               s = heeAdd     s
eval _   (EName "-")               s = heeSub     s
eval _   (EName "*")               s = heeMul     s
eval _   (EName "/")               s = heeDiv     s
eval _   (EName "%")               s = heeMod     s
eval env (EName "if")              s = heeIf      env s
eval _   (EName "or")              s = heeOr      s
eval _   (EName "and")             s = heeAnd     s
eval _   (EName "not")             s = heeNot     s
eval _   (EName "==")              s = heeEq      s
eval _   (EName "/=")              s = heeNe      s
eval _   (EName "<")               s = heeLt      s
eval _   (EName ">")               s = heeGt      s
eval _   (EName "<=")              s = heeLte     s
eval _   (EName ">=")              s = heeGte     s
eval env (EName name)              s = case env name of
                                         Just e  -> eval env e s
                                         Nothing -> Failure (EName name) s
eval _   e s                         = Failure e s

heeId :: Stack -> Result
heeId = Success EEmpty

heePop :: Stack -> Result
heePop (_:xs) = Success EEmpty xs
heePop s      = Failure (EName "pop") s

heeDup :: Stack -> Result
heeDup (x:xs) = Success EEmpty (x:x:xs)
heeDup s      = Failure (EName "dup") s

heeDup2 :: Stack -> Result
heeDup2 (x:y:zs) = Success EEmpty (x:y:x:y:zs)
heeDup2 s        = Failure (EName "dup2") s

heeDig :: Stack -> Result
heeDig (w:x:y:zs) = Success EEmpty (y:w:x:zs)
heeDig s          = Failure (EName "dig") s

heeSwap :: Stack -> Result
heeSwap (x:y:zs) = Success EEmpty (y:x:zs)
heeSwap s        = Failure (EName "swap") s

heeBury :: Stack -> Result
heeBury (w:x:y:zs) = Success EEmpty (x:y:w:zs)
heeBury s          = Failure (EName "bury") s

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

heeApply :: Environment -> Stack -> Result
heeApply env (VQuote e:xs) = eval env e xs
heeApply _   s             = Failure (EName "apply") s

heeDip :: Environment -> Stack -> Result
heeDip env (VQuote e:x:ys) = case eval env e ys of
                               Success e' ys' -> Success e' (x:ys')
                               failure        -> failure
heeDip _   s               = Failure (EName "dip") s

heeU :: Environment -> Stack -> Result
heeU env = eval env (ECompose (EName "dup") (EName "apply"))

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

heeIf :: Environment -> Stack -> Result
heeIf env (VQuote _:VQuote t:VBool True :zs) = eval env t zs
heeIf env (VQuote f:VQuote _:VBool False:zs) = eval env f zs
heeIf _   s                                  = Failure (EName "if") s

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
heeBoth :: Environment -> Stack -> Result
heeBoth env = eval env
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
