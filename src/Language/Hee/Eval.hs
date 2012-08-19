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
  [("u",    ECompose (EName "dup") (EName "apply"))
  ,("both", ECompose
              (EName "dup")
              (ECompose
                (EQuote
                  (ECompose
                    (EName "swap")
                    (ECompose
                      (EQuote (EName "apply"))
                      (EName "dip"))))
                (ECompose (EName "dip") (EName "apply"))))]


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
evalExpr (EName "id")      = evalId        =<< get
evalExpr (EName "pop")     = evalPop       =<< get
evalExpr (EName "dup")     = evalDup       =<< get
evalExpr (EName "dup2")    = evalDup2      =<< get
evalExpr (EName "dig")     = evalDig       =<< get
evalExpr (EName "swap")    = evalSwap      =<< get
evalExpr (EName "bury")    = evalBury      =<< get
evalExpr (EName "quote")   = evalQuote     =<< get
evalExpr (EName "compose") = evalCompose   =<< get
evalExpr (EName "apply")   = evalApply     =<< get
evalExpr (EName "dip")     = evalDip       =<< get
evalExpr (EName "+")       = evalOp (+)    =<< get
evalExpr (EName "-")       = evalOp (-)    =<< get
evalExpr (EName "*")       = evalOp (*)    =<< get
evalExpr (EName "/")       = evalFrac (/)  =<< get
evalExpr (EName "%")       = evalInt mod   =<< get
evalExpr (EName "if")      = evalIf        =<< get
evalExpr (EName "not")     = evalNot       =<< get
evalExpr (EName "or")      = evalBool (||) =<< get
evalExpr (EName "and")     = evalBool (&&) =<< get
evalExpr (EName "==")      = evalEq  (==)  =<< get
evalExpr (EName "/=")      = evalEq  (/=)  =<< get
evalExpr (EName "<")       = evalOrd (<)   =<< get
evalExpr (EName ">")       = evalOrd (>)   =<< get
evalExpr (EName "<=")      = evalOrd (<=)  =<< get
evalExpr (EName ">=")      = evalOrd (>=)  =<< get
evalExpr (EName name)
  = do env <- ask
       case lookup name env of
         Just expr -> evalExpr expr
         Nothing   -> throwError (append "undefined: " name)

evalLit :: Literal -> Eval ()
evalLit (LChar x)       = modify (VChar x:)
evalLit (LString x)     = modify (VString x:)
evalLit (LInteger _ x)  = modify (VInt x:)
evalLit (LFloat x)      = modify (VFloat x:)
evalLit (LBool x)       = modify (VBool x:)

evalId :: Stack -> Eval ()
evalId _ = return ()

evalPop :: Stack -> Eval ()
evalPop (_:zs) = put zs
evalPop  _     = throwError "pop: stack underflow"

evalDup :: Stack -> Eval ()
evalDup (y:zs) = put (y:y:zs)
evalDup _      = throwError "dup: stack underflow"

evalDup2 :: Stack -> Eval ()
evalDup2 (x:y:zs) = put (x:y:x:y:zs)
evalDup2 _        = throwError "dup2: stack underflow"

evalDig :: Stack -> Eval ()
evalDig (w:x:y:zs) = put (y:w:x:zs)
evalDig _          = throwError "dig: stack underflow"

evalSwap :: Stack -> Eval ()
evalSwap (x:y:zs) = put (y:x:zs)
evalSwap _        = throwError "swap: stack underflow"

evalBury :: Stack -> Eval ()
evalBury (w:x:y:zs) = put (x:y:w:zs)
evalBury _          = throwError "bury: stack underflow"

evalQuote :: Stack -> Eval ()
evalQuote (y:zs) = put (VQuote (quote y):zs)
  where
    quote (VChar   c) = ELiteral (LChar c)
    quote (VString c) = ELiteral (LString c)
    quote (VInt c)    = ELiteral (LInteger Decimal c)
    quote (VFloat c)  = ELiteral (LFloat c)
    quote (VQuote  c) = EQuote c
    quote (VBool   c) = ELiteral (LBool c)
evalQuote _ = throwError "quote: stack underflow"

evalCompose :: Stack -> Eval ()
evalCompose (VQuote x:VQuote y:zs) = put (VQuote (ECompose x y):zs)
evalCompose _                      = throwError "compose"

evalApply :: Stack -> Eval ()
evalApply (VQuote y:zs) = put zs >> evalExpr y
evalApply _             = throwError "apply"

evalDip :: Stack -> Eval ()
evalDip (VQuote x:y:zs) = put zs >> evalExpr x >> modify (y:)
evalDip _               = throwError "dip"

evalIf :: Stack -> Eval ()
evalIf (VQuote _:VQuote t:VBool True :zs) = put zs >> evalExpr t
evalIf (VQuote f:VQuote _:VBool False:zs) = put zs >> evalExpr f
evalIf _                                  = throwError "if"

evalOp :: (forall a. Num a => a -> a -> a) -> Stack -> Eval ()
evalOp op (VInt   x:VInt   y:zs) = put ((VInt   $ y `op` x):zs)
evalOp op (VFloat x:VFloat y:zs) = put ((VFloat $ y `op` x):zs)
evalOp _ _                       = throwError "numeric-op"

evalNot :: Stack -> Eval ()
evalNot (VBool y:zs) = put ((VBool $ not y):zs)
evalNot _            = throwError "not"

evalBool :: (Bool -> Bool -> Bool) -> Stack -> Eval ()
evalBool op (VBool x:VBool y:zs) = put ((VBool $ x `op` y):zs)
evalBool _ _                     = throwError "boolean-op"

evalEq :: (forall a. Eq a => a -> a -> Bool) -> Stack -> Eval ()
evalEq op (VChar   x:VChar   y:zs) = put ((VBool $ x `op` y):zs)
evalEq op (VString x:VString y:zs) = put ((VBool $ x `op` y):zs)
evalEq op (VInt    x:VInt    y:zs) = put ((VBool $ x `op` y):zs)
evalEq op (VFloat  x:VFloat  y:zs) = put ((VBool $ x `op` y):zs)
evalEq op (VBool   x:VBool   y:zs) = put ((VBool $ x `op` y):zs)
evalEq _ _                         = throwError "equality-op"

evalOrd :: (forall a. Ord a => a -> a -> Bool) -> Stack -> Eval ()
evalOrd op (VChar   x:VChar   y:zs) = put ((VBool $ x `op` y):zs)
evalOrd op (VString x:VString y:zs) = put ((VBool $ x `op` y):zs)
evalOrd op (VInt    x:VInt    y:zs) = put ((VBool $ x `op` y):zs)
evalOrd op (VFloat  x:VFloat  y:zs) = put ((VBool $ x `op` y):zs)
evalOrd op (VBool   x:VBool   y:zs) = put ((VBool $ x `op` y):zs)
evalOrd _ _                         = throwError "compare-op"

evalInt :: (forall a. Integral a => a -> a -> a) -> Stack -> Eval ()
evalInt op (VInt x:VInt y:zs) = put ((VInt $ x `op` y):zs)
evalInt _ _                   = throwError "integral-op"

evalFrac :: (forall a. Fractional a => a -> a -> a) -> Stack -> Eval ()
evalFrac op (VFloat   x:VFloat   y:zs) = put ((VFloat $ x `op` y):zs)
evalFrac _ _                           = throwError "fractional-op"
