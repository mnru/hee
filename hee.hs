import List (nub, (\\), intersect, union, partition)
import Monad (msum)

type Id = String

type TVar = (Id, Kind)
type TAbs = (Id, Kind)

data Kind = KStar
          | KAbs Kind Kind
          deriving (Eq, Show)

data Type = TVar TVar
          | TAbs TAbs
          | TApp Type Type
          | TGen Int
          deriving (Eq, Show)

data Expr = EEmpty
          | EName Id
          | EQuote [Expr]
          | ECompose Expr Expr
          deriving (Eq, Show)

-- Primitive data types
tUnit   = TAbs ("()"     ,KStar)
tChar   = TAbs ("char"   ,KStar)
tInt    = TAbs ("int"    ,KStar)
tFloat  = TAbs ("float"  ,KStar)
tDouble = TAbs ("double" ,KStar)
tList   = TAbs ("[]"    ,(KAbs KStar KStar))
tArrow  = TAbs ("->"    ,(KAbs KStar (KAbs KStar KStar)))
tPair   = TAbs ("(,)"   ,(KAbs KStar (KAbs KStar KStar)))
tTriple = TAbs ("(,,)"  ,(KAbs KStar (KAbs KStar (KAbs KStar KStar))))

-- Helper functions
fn a b   = TApp (TApp tArrow a) b
pair a b = TApp (TApp tPair a) b
list t   = TApp tList t

-- Overloaded function `kind` that can be used to determine the kind of
-- a type variable, type constant, or type expression
class HasKind t where
  kind :: t -> Kind
instance HasKind Type where
  kind (TVar (_, k)) = k
  kind (TAbs (_, k)) = k
  -- We can calculate the kind of an application, it's the result side of t
  kind (TApp t _) = case (kind t) of (KAbs _ k) -> k

-- Substitutions are represented using association lists
type Substitution = [(TVar, Type)]

nullSub  :: Substitution
nullSub   = []

(@@)     :: Substitution -> Substitution -> Substitution
(@@) a b  = [(u, substitute a t) | (u, t) <- b] ++ a

merge    :: Monad m => Substitution -> Substitution -> m Substitution
merge a b = if agree
            then return (a ++ b)
            else fail "merge failed"
  where agree = all (\v -> substitute a (TVar v) == substitute b (TVar v))
                    (map fst a `intersect` map fst b)

-- Substitutions can be applied to types or anything with type components
class CanSubstitute t where
  substitute :: Substitution -> t -> t
  tvars      :: t -> [TVar]

instance CanSubstitute Type where
  substitute s (TVar u)   = case lookup u s of
                              Just t  -> t
                              Nothing -> TVar u
  substitute s (TApp a b) = TApp (substitute s a) (substitute s b)
  substitute s t          = t

  tvars (TVar v)   = [v]
  tvars (TApp a b) = tvars a `union` tvars b
  tvars _          = []

-- Extend these operations to work on lists
instance CanSubstitute a => CanSubstitute [a] where
  substitute s = map (substitute s)
  tvars        = nub . concat . map tvars

-- Unification is a partial function, so results are in a monad. This
-- finds a Substitution s such that (substitute s a) == (substitute s b)
unify                        :: Monad m => Type -> Type -> m Substitution
unify (TApp a b) (TApp a' b') = do sb <- unify b b'
                                   sa <- unify (substitute sb a) (substitute sb a')
                                   return ((@@) sa sb)
unify (TVar u) t              = bindTVar u t
unify t (TVar u)              = bindTVar u t
unify (TAbs a) (TAbs b)
  | a == b                    = return nullSub
unify _ _                     = fail "unify failed"

-- Special case of unifying a type variable with a type, ensuring validity
bindTVar :: Monad m => TVar -> Type -> m Substitution
bindTVar v@(_,k) t | t == TVar v      = return nullSub
                   | v `elem` tvars t = fail "occurs check failed"
                   | k /= kind t      = fail "kinds do not match"
                   | otherwise        = return [(v, t)]

-- Finds a substitution s such that (substitute s a) == b. Follows the same
-- pattern as unification, except it uses `merge` instead of `@@` to combine
-- substitutions, and it does not allow binding of variables in b
match                        :: Monad m => Type -> Type -> m Substitution
match (TApp a b) (TApp a' b') = do sb <- match b b'
                                   sa <- match a a'
                                   merge sa sb
match (TVar v@(_,k)) t
  | k == kind t               = return [(v, t)]
match (TAbs a) (TAbs b)
  | a == b                    = return nullSub
match _ _                     = fail "types do not match"
