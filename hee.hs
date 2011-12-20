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

-- Construct an empty substitution
nullSub  :: Substitution
nullSub   = []

-- Construct a singleton substitution
(+->)    :: TVar -> Type -> Substitution
(+->) v t = [(v, t)]

-- Compose two substitutions, but bindings in b take precedence over a
(@@)     :: Substitution -> Substitution -> Substitution
(@@) a b  = [(v, substitute a t) | (v, t) <- b] ++ a

-- Compose two substitutions if binding precedence is irrelevant
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
  substitute s (TVar v)   = case lookup v s of
                              Just t  -> t
                              Nothing -> TVar v
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
                                   return sa `@@` sb
unify (TVar v) t              = bindTVar v t
unify t (TVar v)              = bindTVar v t
unify (TAbs a) (TAbs b)
  | a == b                    = return nullSub
unify _ _                     = fail "unify failed"

-- Special case of unifying a type variable with a type, ensuring validity
bindTVar :: Monad m => TVar -> Type -> m Substitution
bindTVar v@(_,k) t | t == TVar v      = return nullSub
                   | v `elem` tvars t = fail "occurs check failed"
                   | k /= kind t      = fail "kinds do not match"
                   | otherwise        = return v +-> t

-- Finds a substitution s such that (substitute s a) == b. Follows the same
-- pattern as unification, except it uses `merge` instead of `@@` to combine
-- substitutions, and it does not allow binding of variables in b
match                        :: Monad m => Type -> Type -> m Substitution
match (TApp a b) (TApp a' b') = do sb <- match b b'
                                   sa <- match a a'
                                   sa `merge` sb
match (TVar v@(_,k)) t
  | k == kind t               = return v +-> t
match (TAbs a) (TAbs b)
  | a == b                    = return nullSub
match _ _                     = fail "types do not match"

-------------------------------------------------------------------------------

data Qualified t = [Predicate] :=> t
                   deriving Eq

data Predicate = MemberOf Id Type
                 deriving Eq

instance CanSubstitute t => CanSubstitute (Qualified t) where
  substitute s (ps :=> t) = substitute s ps :=> substitute s t
  tvars (ps :=> t)        = tvars ps `union` tvars t

instance CanSubstitute Predicate where
  substitute s (MemberOf id t) = MemberOf id (substitute s t)
  tvars (MemberOf id t)        = tvars t

unifyPredicate :: Predicate -> Predicate -> Maybe Substitution
unifyPredicate  = lift unify

matchPredicate :: Predicate -> Predicate -> Maybe Substitution
matchPredicate  = lift match

lift m (MemberOf i t) (MemberOf i' t') 
  | i == i'   = m t t'
  | otherwise = fail "classes do not match"

type Class    = ([Id], [Instance])
type Instance = Qualified Predicate

-- The Haskell class Ord might be described by:
--   Eq is a superclass of Ord
--   Eq has four instances
--
-- (["Eq"], [ [] :=> MemberOf "Ord" tUnit,
--            [] :=> MemberOf "Ord" tChar,
--            [] :=> MemberOf "Ord" tInt,
--            [ MemberOf "Ord" (TVar ("a", Star))
--            , MemberOf "Ord" (TVar ("b", Star)) ]
--               :=> MemberOf "Ord" (pair (TVar ("a", Star))
--                                        (TVar ("b", Star))) ])

-- TODO: Store additional information for each declaration, such
-- as list of member functions for each class and details of their
-- implementations in each particular instance

