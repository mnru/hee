-- Adapted from Mark P. Jones' "Typing Haskell in Haskell"
--   http://web.cecs.pdx.edu/~mpj/thih/thih.pdf

import List (nub, (\\), intersect, union, partition)
import Monad (msum)

type Id = String

-- Terms are either a composition of one function with another term, or
-- a higher-order function (a string of composed functions)
data Term = TmCompose Term Id
          | TmQuote Term
          | TmEmpty
  deriving (Eq, Show)

-- Kinds classify types as either a monomorphic value type (a nullary type
-- constructor), a unary type constructor (type => type), or a stack
data Kind = KiStack
          | KiType
          | KiCons Kind Kind
  deriving (Eq, Show)

-- Types have the kind KiType and are distinguished from Stack because
-- they can be used to describe first-class values.
data Type = TyVariable Id Kind
          | TyConstant Id Kind
          | TyApplication Type Type
          | TyStack Stack
  deriving (Eq, Show)

-- Stacks have the kind KiStack and are distinguished from Type because
-- they cannot be used to describe first-class values.
data Stack = StEmpty
           | StBottom Id
           | StPush Stack Type
  deriving (Eq, Show)

showTerm :: Term -> String
showTerm TmEmpty                = ""
showTerm (TmQuote t)            = "[" ++ showTerm t ++ "]"
showTerm (TmCompose TmEmpty u)  = u
showTerm (TmCompose t u)        = showTerm t ++ " " ++ u

showKind :: Kind -> String
showKind KiStack                    = "@"
showKind KiType                     = "*"
showKind (KiCons k@(KiCons _ _) k') = "(" ++ showKind k ++ ") => " ++ showKind k'
showKind (KiCons k k')              = showKind k ++ " => " ++ showKind k'

showType :: Type -> String
showType (TyVariable id k)    = id
showType (TyConstant id k)    = id
showType (TyApplication t t') = "(" ++ showType t ++ ") " ++ showType t'
showType (TyStack s)          = "(" ++ showStack s ++ ")"

showStack :: Stack -> String
showStack StEmpty       = "|"
showStack (StBottom id) = id
showStack (StPush s s') = showStack s ++ "> " ++ showType s'

-- Primitive types
tInt  = TyConstant "int"  KiType
tReal = TyConstant "real" KiType
tChar = TyConstant "char" KiType
tUnit = TyConstant "()"   KiType
tPair = TyConstant "(,)"  (KiCons KiType (KiCons KiType KiType))
tList = TyConstant "[]"   (KiCons KiType KiType)
tFunc = TyConstant "(->)" (KiCons KiStack KiStack)

mkFunc :: Stack -> Stack -> Type
mkFunc inp out = TyApplication (TyApplication tFunc (TyStack inp)) (TyStack out)

mkList :: Type -> Type
mkList t = TyApplication tList t

mkPair :: Type -> Type -> Type
mkPair fst snd = TyApplication (TyApplication tPair fst) snd

class HasKind t where
  kind :: t -> Kind

instance HasKind Type where
  kind (TyVariable _ k)     = k
  kind (TyConstant _ k)     = k
  kind (TyStack _)          = KiStack
  kind (TyApplication i _)  = case (kind i) of
                                (KiCons _ k) -> k
instance HasKind Kind where
  kind k = k

instance HasKind Stack where
  kind t = KiStack

-- TyVariable -> Type
type Variable     = (Id, Kind)
type Substitution = [(Variable, Type)]

-- Empty subtitution
nullSubstitution :: Substitution
nullSubstitution = []

-- Singleton substitution
--   kind preserving iff (kind v) == (kind t)
(+->)    :: Variable -> Type -> Substitution
(+->) v t = [(v, t)]

class CanSubstitute t where
  substitute :: Substitution -> t -> t
  freeVars   :: t -> [Variable]

instance CanSubstitute Type where
  substitute s (TyApplication i o) = TyApplication (substitute s i) (substitute s o)
  substitute s (TyStack t)         = TyStack (substitute s t)
  substitute s (TyVariable id k)   = case lookup (id,k) s of
                                       Just t  -> t
                                       Nothing -> TyVariable id k
  substitute s t = t
  
  freeVars (TyApplication i o) = freeVars i `union` freeVars o
  freeVars (TyStack t)         = freeVars t
  freeVars (TyVariable id k)   = [(id,k)]
  freeVars _                   = []

instance CanSubstitute Stack where
  substitute s (StBottom id) = case lookup (id,KiStack) s of
                                 Just (TyStack t) -> t
                                 Just t  -> StBottom id
                                 Nothing -> StBottom id
  substitute s t = t
  
  freeVars (StBottom id) = [(id,KiStack)]
  freeVars _             = []

instance CanSubstitute a => CanSubstitute [a] where
  substitute s = map (substitute s)
  freeVars     = nub . concat . map freeVars

-- Composition of substitutions
--   substitute (a @@ b) = substitute a . substitute b
infixr   4 @@
(@@)    :: Substitution -> Substitution -> Substitution
(@@) a b = [(v, substitute a t) | (v, t) <- b] ++ b

-- Composition of substitutions
merge    :: Monad m => Substitution -> Substitution -> m Substitution
merge a b = if match
            then return (a ++ b)
            else fail "merge failed"
  where match = all (\v -> substitute a (tvar v) == substitute b (tvar v))
                    (map fst a `intersect` map fst b)
        tvar (id,k) = TyVariable id k
        svar (id,k) = StBottom id
