{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Language.Hee.Eval
  ( runFile
  , Value(..)
  ) where

import Prelude hiding (lookup)
import Data.Map hiding (map)
import Data.Text (Text, append, pack)
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error

import Language.Hee.Pretty (renderString)
import Language.Hee.Syntax hiding (Stack)

data Value
  = VChar Char
  | VString Text
  | VInt Int
  | VFloat Float
  | VQuote Expression
  | VBool Bool
  deriving (Eq)

instance Show Value where
  show (VChar v)    = renderString (LChar v)
  show (VString v)  = renderString (LString v)
  show (VInt v)     = renderString (LInteger Decimal v)
  show (VFloat v)   = renderString (LFloat v)
  show (VQuote v)   = renderString (EQuote v)
  show (VBool v)    = renderString (LBool v)

instance Error Text where
  noMsg  = ""
  strMsg = pack

-- Things
--------------------------------------------------------------------------------

type Stack
  = [Value]

defaultStack :: Stack
defaultStack = []

type Environment
  = Map Text Expression

buildEnv :: [Declaration] -> Environment
buildEnv = fromList . map (\(DNameBind name _ _ expr) -> (name, expr))

mergeEnv :: Environment -> Environment -> Environment
mergeEnv = union

defaultEnv :: Environment
defaultEnv = fromList
  [("u",    ECompose (EName "dup") (EName "unquote"))
  ,("both", ECompose
              (EName "dup")
              (ECompose
                (EQuote
                  (ECompose
                    (EName "swap")
                    (ECompose
                      (EQuote (EName "unquote"))
                      (EName "dip"))))
                (ECompose (EName "dip") (EName "unquote"))))]


-- Evaluation
--------------------------------------------------------------------------------

type Eval a
  = ReaderT Environment (ErrorT Text (WriterT [Text] (StateT Stack Identity))) a

runEval :: Environment -> Stack -> Eval a -> ((Either Text a, [Text]), Stack)
runEval env stack computation
  = runIdentity (runStateT (runWriterT (runErrorT (runReaderT computation env))) stack)

runFile :: [Declaration] -> ((Either Text (), [Text]), Stack)
runFile ds = runEval env stack (evalExpr $ EName "main")
  where
    env   = mergeEnv defaultEnv (buildEnv ds)
    stack = defaultStack

evalExpr :: Expression -> Eval ()
evalExpr EEmpty            = return ()
evalExpr (ECompose f g)    = evalExpr f >> evalExpr g
evalExpr (EQuote e)        = modify (VQuote e:)
evalExpr (ELiteral e)      = evalLit e

-- Stack operators
evalExpr (EName "id")      = evalId
evalExpr (EName "pop")     = evalPop
evalExpr (EName "dup")     = evalDup
evalExpr (EName "dup2")    = evalDup2
evalExpr (EName "dig")     = evalDig
evalExpr (EName "swap")    = evalSwap
evalExpr (EName "bury")    = evalBury

-- Function operators
evalExpr (EName "quote")   = evalQuote
evalExpr (EName "compose") = evalCompose
evalExpr (EName "unquote") = evalUnquote
evalExpr (EName "dip")     = evalDip

-- Numeric operators
evalExpr (EName "+")       = evalOp (+)
evalExpr (EName "-")       = evalOp (-)
evalExpr (EName "*")       = evalOp (*)
evalExpr (EName "/")       = evalFrac (/)
evalExpr (EName "%")       = evalInt mod
evalExpr (EName "//")      = evalInt div
evalExpr (EName "^")       = evalExp
evalExpr (EName "round")   = evalRound

-- Boolean operators
evalExpr (EName "if")      = evalIf
evalExpr (EName "not")     = evalNot
evalExpr (EName "or")      = evalBool (||)
evalExpr (EName "and")     = evalBool (&&)

-- Comparison operators
evalExpr (EName "==")      = evalEq  (==)
evalExpr (EName "/=")      = evalEq  (/=)
evalExpr (EName "<")       = evalOrd (<)
evalExpr (EName ">")       = evalOrd (>)
evalExpr (EName "<=")      = evalOrd (<=)
evalExpr (EName ">=")      = evalOrd (>=)
evalExpr (EName name)
  = do env <- ask
       case lookup name env of
         Just expr -> evalExpr expr
         Nothing   -> throwError (append "undefined: " name)
evalExpr e = throwError (pack (show e))

evalLit :: Literal -> Eval ()
evalLit (LChar x)       = modify (VChar x:)
evalLit (LString x)     = modify (VString x:)
evalLit (LInteger _ x)  = modify (VInt x:)
evalLit (LFloat x)      = modify (VFloat x:)
evalLit (LBool x)       = modify (VBool x:)

evalId :: Eval ()
evalId = return ()

evalPop :: Eval ()
evalPop = f =<< get
  where f (_:zs) = put zs
        f  _     = throwError "pop: stack underflow"

evalDup :: Eval ()
evalDup = f =<< get
  where f (y:zs) = put (y:y:zs)
        f _      = throwError "dup: stack underflow"

evalDup2 :: Eval ()
evalDup2 = f =<< get
  where f (x:y:zs) = put (x:y:x:y:zs)
        f _        = throwError "dup2: stack underflow"

evalDig :: Eval ()
evalDig = f =<< get
  where f (w:x:y:zs) = put (y:w:x:zs)
        f _          = throwError "dig: stack underflow"

evalSwap :: Eval ()
evalSwap = f =<< get
  where f (x:y:zs) = put (y:x:zs)
        f _        = throwError "swap: stack underflow"

evalBury :: Eval ()
evalBury = f =<< get
  where f (w:x:y:zs) = put (x:y:w:zs)
        f _          = throwError "bury: stack underflow"

evalQuote :: Eval ()
evalQuote = f =<< get
  where f (y:zs) = put (VQuote (quote y):zs)
        f _      = throwError "quote: stack underflow"
        quote (VChar   c) = ELiteral (LChar c)
        quote (VString c) = ELiteral (LString c)
        quote (VInt c)    = ELiteral (LInteger Decimal c)
        quote (VFloat c)  = ELiteral (LFloat c)
        quote (VQuote  c) = EQuote c
        quote (VBool   c) = ELiteral (LBool c)

evalCompose :: Eval ()
evalCompose = f =<< get
  where f (VQuote x:VQuote y:zs) = put (VQuote (ECompose x y):zs)
        f _                      = throwError "compose"

evalUnquote :: Eval ()
evalUnquote = f =<< get
  where f (VQuote y:zs) = put zs >> evalExpr y
        f _             = throwError "unquote"

evalDip :: Eval ()
evalDip = f =<< get
  where f (VQuote x:y:zs) = put zs >> evalExpr x >> modify (y:)
        f _               = throwError "dip"

evalIf :: Eval ()
evalIf = f =<< get
  where f (VQuote   _:VQuote tru:VBool True :zs) = put zs >> evalExpr tru
        f (VQuote fls:VQuote   _:VBool False:zs) = put zs >> evalExpr fls
        f _                                      = throwError "if"

evalOp :: (forall a. Num a => a -> a -> a) -> Eval ()
evalOp op = f =<< get
  where f (VInt   x:VInt   y:zs) = put ((VInt   $ y `op` x):zs)
        f (VFloat x:VFloat y:zs) = put ((VFloat $ y `op` x):zs)
        f _                      = throwError "numeric-op"

evalNot :: Eval ()
evalNot = f =<< get
  where f (VBool y:zs) = put ((VBool $ not y):zs)
        f _            = throwError "not"

evalBool :: (Bool -> Bool -> Bool) -> Eval ()
evalBool op = f =<< get
  where f (VBool x:VBool y:zs) = put ((VBool $ x `op` y):zs)
        f _                    = throwError "boolean-op"

evalEq :: (forall a. Eq a => a -> a -> Bool) -> Eval ()
evalEq op = f =<< get
  where f (VChar   x:VChar   y:zs) = put ((VBool $ x `op` y):zs)
        f (VString x:VString y:zs) = put ((VBool $ x `op` y):zs)
        f (VInt    x:VInt    y:zs) = put ((VBool $ x `op` y):zs)
        f (VFloat  x:VFloat  y:zs) = put ((VBool $ x `op` y):zs)
        f (VBool   x:VBool   y:zs) = put ((VBool $ x `op` y):zs)
        f _                        = throwError "equality-op"

evalOrd :: (forall a. Ord a => a -> a -> Bool) -> Eval ()
evalOrd op = f =<< get
  where f (VChar   x:VChar   y:zs) = put ((VBool $ y `op` x):zs)
        f (VString x:VString y:zs) = put ((VBool $ y `op` x):zs)
        f (VInt    x:VInt    y:zs) = put ((VBool $ y `op` x):zs)
        f (VFloat  x:VFloat  y:zs) = put ((VBool $ y `op` x):zs)
        f (VBool   x:VBool   y:zs) = put ((VBool $ y `op` x):zs)
        f _                        = throwError "compare-op"

evalInt :: (forall a. Integral a => a -> a -> a) -> Eval ()
evalInt op = f =<< get
  where f (VInt x:VInt y:zs) = put ((VInt $ y `op` x):zs)
        f _                  = throwError "integral-op"

evalExp :: Eval ()
evalExp = f =<< get
  where f (VInt x:VInt   y:zs) = put ((VInt   $ y ^ x):zs)
        f (VInt x:VFloat y:zs) = put ((VFloat $ y ^ x):zs)
        f _                    = throwError "^"

evalRound :: Eval ()
evalRound = f =<< get
  where f (VFloat y:zs) = put ((VInt $ round y):zs)
        f _             = throwError "round"

evalFrac :: (forall a. Fractional a => a -> a -> a) -> Eval ()
evalFrac op = f =<< get
  where f (VFloat   x:VFloat   y:zs) = put ((VFloat $ y `op` x):zs)
        f _                          = throwError "fractional-op"
