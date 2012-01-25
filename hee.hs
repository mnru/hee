-- Adapted from Mark P. Jones' "Typing Haskell in Haskell"
--   http://web.cecs.pdx.edu/~mpj/thih/thih.pdf

import List (nub, (\\), intersect, union, partition)
import Monad (msum)
import Maybe (isJust)
import qualified Data.Foldable as F (concat)

type Id
  = String

-- Terms are either a composition of two functions, or a higher-order
-- function (a string of composed functions)
data Term
  = TmCompose Term Term
  | TmQuote Term
  | TmEmpty
  | TmName Id
  | TmLiteral Literal
  deriving (Eq, Show)

data Literal
  = LiInt Int
  | LiChar Char
  | LiFloat Float
  | LiString String
  deriving (Eq, Show)

-- Kinds classify types as either a monomorphic value type (a nullary type
-- constructor), a unary type constructor (type => type), or a stack
data Kind
  = KiStack
  | KiType
  | KiCons Kind Kind
  deriving (Eq, Show)

-- Types have the kind KiType and are distinguished from Stack because
-- they can be used to describe first-class values.
data Type
  = TyVariable Id Kind
  | TyConstant Id Kind
  | TyApplication Type Type
  | TyGeneric Int
  | TyStack Stack
  deriving (Eq, Show)

-- Stacks have the kind KiStack and are distinguished from Type because
-- they cannot be used to describe first-class values.
data Stack
  = StEmpty
  | StBottom Id
  | StPush Stack Type
  deriving (Eq, Show)

showTerm :: Term -> String
showTerm TmEmpty                = ""
showTerm (TmName id)            = id
showTerm (TmQuote t)            = "[" ++ showTerm t ++ "]"
showTerm (TmLiteral t)          = showLit t
showTerm (TmCompose TmEmpty t)  = showTerm t
showTerm (TmCompose s t)        = showTerm s ++ " " ++ showTerm t

showLit :: Literal -> String
showLit (LiInt l)    = show l
showLit (LiChar l)   = show l
showLit (LiFloat l)  = show l
showLit (LiString l) = show l

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
tInt    = TyConstant "int"  KiType
tReal   = TyConstant "real" KiType
tChar   = TyConstant "char" KiType
tUnit   = TyConstant "()"   KiType
tPair   = TyConstant "(,)"  (KiCons KiType (KiCons KiType KiType))
tList   = TyConstant "[]"   (KiCons KiType KiType)
tFunc   = TyConstant "(->)" (KiCons KiStack KiStack)
tString = mkList tChar

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
  kind (TyApplication i _)  = let (KiCons _ k) = kind i in k

instance HasKind Kind where
  kind k = k

instance HasKind Stack where
  kind t = KiStack

type Variable
  = (Id,Kind)

type Substitution
  = [(Variable, Type)]

-- HasKind Variable
instance (HasKind b) => HasKind (a,b) where
  kind (_,k) = kind k

-- Empty subtitution
nullSubstitution :: Substitution
nullSubstitution = []

-- Singleton substitution
--   kind preserving iff (kind v) == (kind t)
(+->)    :: Variable -> Type -> Substitution
(+->) v t = [(v, t)]

class CanSubstitute t where
  substitute :: Substitution -> t -> t
  freevars   :: t -> [Variable]

instance CanSubstitute Type where
  substitute s (TyApplication i o) = TyApplication (substitute s i) (substitute s o)
  substitute s (TyStack t)         = TyStack (substitute s t)
  substitute s (TyVariable id k)   = case lookup (id,k) s of
                                       Just t  -> t
                                       Nothing -> TyVariable id k
  substitute s t = t

  freevars (TyApplication i o) = freevars i `union` freevars o
  freevars (TyStack t)         = freevars t
  freevars (TyVariable id k)   = [(id,k)]
  freevars _                   = []

instance CanSubstitute Stack where
  substitute s (StBottom id) = case lookup (id,KiStack) s of
                                 Just (TyStack t) -> t
                                 Just t  -> StBottom id
                                 Nothing -> StBottom id
  substitute s t = t

  freevars (StBottom id) = [(id,KiStack)]
  freevars _             = []

instance CanSubstitute a => CanSubstitute [a] where
  substitute s = map (substitute s)
  freevars     = nub . concat . map freevars

-- Composition of substitutions
--   substitute (a @@ b) = substitute a . substitute b
infixr   4 @@
(@@)    :: Substitution -> Substitution -> Substitution
(@@) a b = [(v, substitute a t) | (v, t) <- b] ++ b

-- Composition of substitutions
merge    :: Monad m => Substitution -> Substitution -> m Substitution
merge a b = if all match (map fst a `intersect` map fst b)
            then return (a ++ b)
            else fail "merge failed"
  where match (id,KiStack) = substitute a (StBottom id)     == substitute b (StBottom id)
        match (id,k)       = substitute a (TyVariable id k) == substitute b (TyVariable id k)

class CanUnify t where
  match   :: Monad m => t -> t -> m Substitution
  unify   :: Monad m => t -> t -> m Substitution
  bindvar :: Monad m => Variable -> t -> m Substitution

instance CanUnify Type where
  match (TyStack s) (TyStack s') = match s s'
  match (TyVariable id k) t      = bindvar (id,k) t
  match (TyConstant id k) (TyConstant id' k')
    | id == id' && k == k'       = return nullSubstitution
  match (TyApplication i o) (TyApplication i' o')
                                 = do a <- match i i'
                                      b <- match (substitute a o) (substitute a o')
                                      merge a b
  match _ _                      = fail "merge failed"

  unify (TyStack s) (TyStack s') = unify s s'
  unify (TyVariable id k) t      = bindvar (id,k) t
  unify t (TyVariable id k)      = bindvar (id,k) t
  unify (TyConstant id k) (TyConstant id' k')
    | id == id' && k == k'       = return nullSubstitution
  unify (TyApplication i o) (TyApplication i' o')
                                 = do a <- unify i i'
                                      b <- unify (substitute a o) (substitute a o')
                                      return (a @@ b)
  unify _ _                      = fail "unify failed"

  bindvar v@(id,k) t
    | t == TyVariable id k = return nullSubstitution
    | v `elem` freevars t  = fail "bindvar failed (occurs check)"
    | k /= kind t          = fail "bindvar failed (kind mismatch)"
    | otherwise            = return (v +-> t)

instance CanUnify Stack where
  match (StBottom id) t = bindvar (id,KiStack) t
  match StEmpty StEmpty = return nullSubstitution
  match (StPush t s) (StPush t' s')
                        = do a <- match t t'
                             b <- match (substitute a s) (substitute a s')
                             merge a b

  unify (StBottom id) t = bindvar (id,KiStack) t
  unify t (StBottom id) = bindvar (id,KiStack) t
  unify StEmpty StEmpty = return nullSubstitution
  unify (StPush t s) (StPush t' s')
                        = do a <- unify t t'
                             b <- unify (substitute a s) (substitute a s')
                             return (a @@ b)
  unify _ _             = fail "unify failed"

  bindvar v@(id,KiStack) t = return (v +-> TyStack t)
  bindvar _ _              = fail "bindvar failed (kind mismatch)"

-- antecedents => consequent
data Qualified h
  = [Predicate] :=> h
  deriving (Eq, Show)

data Predicate
  = MemberOf Id Type
  deriving (Eq, Show)

-- (Num a) => a -> Int
-- [MemberOf "Num" (TyVariable "a" KiType)]
--   :=> (TyVariable "a" KiStar) `mkFunc` tInt

instance CanSubstitute t => CanSubstitute (Qualified t) where
  substitute s (ps :=> h) = substitute s ps :=> substitute s h
  freevars (ps :=> h)     = freevars ps `union` freevars h

instance CanSubstitute Predicate where
  substitute s (MemberOf id t) = MemberOf id (substitute s t)
  freevars (MemberOf id t)     = freevars t

instance CanUnify Predicate where
  match (MemberOf id t) (MemberOf id' t')
    | id == id' = match t t'
    | otherwise = fail "match failed (class mismatch)"

  unify (MemberOf id t) (MemberOf id' t')
    | id == id' = unify t t'
    | otherwise = fail "unify failed (class mismatch)"

  bindvar v t = undefined

-- Names of each superclass, instance declarations
type Class
  = ([Id], [Instance])

type Instance
  = Qualified Predicate

-- class Eq a => Ord a                  where ...
-- instance Ord Unit                    where ...
-- instance Ord Char                    where ...
-- instance Ord Int                     where ...
-- instance (Ord a, Ord b) => Ord (a,b) where ...
--
-- (["Eq"]
-- ,[[] :=> MemberOf "Ord" tUnit
--  ,[] :=> MemberOf "Ord" tChar
--  ,[] :=> MemberOf "Ord" tInt
--  ,[MemberOf "Ord" (TyVariable "a" KiType)
--   ,MemberOf "Ord" (TyVariable "b" KiType)]
--   :=> MemberOf "Ord" (mkPair (TyVariable "a" KiType)
--                              (TyVariable "b" KiType))])

data ClassEnv
  = ClassEnv { classes  :: Id -> Maybe Class }

nullClassEnv :: ClassEnv
nullClassEnv  = ClassEnv { classes = \id -> fail "class not defined" }

modify :: ClassEnv -> Id -> Class -> ClassEnv
modify ce id c = ce { classes = \id' -> if id == id'
                                        then Just c
                                        else classes ce id' }

supers      :: ClassEnv -> Id -> [Id]
supers ce id = F.concat $ fst `fmap` classes ce id

instances      :: ClassEnv -> Id -> [Instance]
instances ce id = F.concat $ snd `fmap` classes ce id

type ClassEnvT = ClassEnv -> Maybe ClassEnv

-- More meaningful name
defined = isJust

infixr       5 <:>
(<:>)       :: ClassEnvT -> ClassEnvT -> ClassEnvT
(<:>) f g ce = f ce >>= g

addClass :: Id -> [Id] -> ClassEnvT
addClass id supers ce
  | defined (classes ce id)                 = fail "class already defined"
  | any (not . defined . classes ce) supers = fail "superclass not defined"
  | otherwise                               = return (modify ce id (supers, []))

addCoreClasses :: ClassEnvT
addCoreClasses  = addClass "Eq"         []
              <:> addClass "Ord"        ["Eq"]
              <:> addClass "Show"       []
              <:> addClass "Read"       []
              <:> addClass "Bounded"    []
              <:> addClass "Enum"       []
              <:> addClass "Functor"    []
              <:> addClass "Monad"      []
              <:> addClass "Num"        ["Eq", "Show"]
              <:> addClass "Real"       ["Num", "Ord"]
              <:> addClass "Fractional" ["Num"]
              <:> addClass "Integral"   ["Real", "Enum"]
              <:> addClass "RealFrac"   ["Real", "Fractional"]
              <:> addClass "Floating"   ["Fractional"]
              <:> addClass "RealFloat"  ["Real", "Floating"]

-- instance (Ord a, Ord b) => Ord (a,b) where...
--
-- addInstance [MemberOf "Ord" (TyVariable "a" KiType)
--             ,MemberOf "Ord" (TyVariable "b" KiType)]
--             MemberOf "Ord" (mkPair (TyVariable "a" KiType)
--                                    (TyVariable "b" KiType))
addInstance :: [Predicate] -> Predicate -> ClassEnvT
addInstance ps p@(MemberOf id t) ce
  | not . defined $ classes ce id = fail "class not defined"
  | any (overlap p) ps            = fail "overlapping instance"
  | otherwise                     = return (modify ce id c)
  where is = instances ce id
        qs = [q | (ps :=> q) <- is]
        c  = (supers ce id, (ps :=> p):is)

overlap    :: Predicate -> Predicate -> Bool
overlap p q = defined $ unify p q
  where defined (Just _) = True
        defined Nothing  = False

addCoreInstances :: ClassEnvT
addCoreInstances  = addInstance [] (MemberOf "Eq" tUnit)
                <:> addInstance [] (MemberOf "Eq" tChar)
                <:> addInstance [] (MemberOf "Eq" tInt)
                <:> addInstance [MemberOf "Eq" (TyVariable "a" KiType)
                                ,MemberOf "Eq" (TyVariable "b" KiType)]
                                (MemberOf "Eq" (mkPair (TyVariable "a" KiType)
                                                       (TyVariable "b" KiType)))
                <:> addInstance [] (MemberOf "Ord" tUnit)
                <:> addInstance [] (MemberOf "Ord" tChar)
                <:> addInstance [] (MemberOf "Ord" tInt)
                <:> addInstance [MemberOf "Ord" (TyVariable "a" KiType)
                                ,MemberOf "Ord" (TyVariable "b" KiType)]
                                (MemberOf "Ord" (mkPair (TyVariable "a" KiType)
                                                        (TyVariable "b" KiType)))

-- Unchecked assumptions:
-- * superclasses should have the same kind as the class
-- * left side of Qualified should only have MemberOf Id TyVariable
-- * type variables in predicate must also appear in the consequent
-- * head must be type constructor applied to distinct variables

-- Produces a flat list of predicates from superclass tree
consequents :: ClassEnv -> Predicate -> [Predicate]
consequents ce p@(MemberOf id t)
  = p : concat [consequents ce (MemberOf id' t) | id' <- supers ce id]

-- Antecedents (from *one* instance) that sufficiently support the given predicate
--   antecedents ce (MemberOf "Ord" (mkPair tInt tChar))
--   = Just [MemberOf "Ord" tInt, MemberOf "Ord" tChar]
antecedents :: ClassEnv -> Predicate -> Maybe [Predicate]
antecedents ce p@(MemberOf id t) = msum [instantiate it | it <- instances ce id]
  where instantiate (ps :=> h) = (\u -> map (substitute u) ps) `fmap` (match h p)

-- True iff p holds when all ps are satisfied. Either p is (1) trivially
-- in ps, (2) semi-trivially in a superclass of one of ps, or (3) p is
-- the consequent of an instance (qs :=> p') in ce, whose antecedents
-- are, in turn, entailed by ps
--   1. entail ce [MemberOf "Ord" tInt] (MemberOf "Ord" tInt)
--   2. entail ce [MemberOf "Ord" tInt] (MemberOf "Eq"  tInt)
--   3. entail ce [MemberOf "Eq" tInt ,MemberOf "Eq" tChar]
--                (MemberOf "Ord" (mkPair tInt tChar))
entail :: ClassEnv -> [Predicate] -> Predicate -> Bool
entail ce ps p = if any (p `elem`) (map (consequents ce) ps)
                 then True
                 else case antecedents ce p of
                        Nothing -> False
                        Just qs -> all (entail ce ps) qs

data Scheme
  = ForAll [Kind] (Qualified Type)
  deriving (Eq, Show)

-- No need to worry about variable capture, nice!
instance CanSubstitute Scheme where
  substitute s (ForAll ks qt) = ForAll ks (substitute s qt)
  freevars (ForAll ks qt)     = freevars qt

qualify      :: [Variable] -> Qualified Type -> Scheme
qualify vs qt = ForAll ks (substitute s qt)
  where vs' = vs `intersect` freevars qt
        ks  = map kind vs'
        s   = zip vs' (map TyGeneric [0..])

data Assumption
  = Id :>: Scheme
  deriving (Eq, Show)

instance CanSubstitute Assumption where
  substitute s (id :>: sc) = id :>: (substitute s sc)
  freevars (id :>: sc)     = freevars sc

--
lookupScheme                     :: Id -> [Assumption] -> Maybe Scheme
lookupScheme id []                = fail "unbound identifier"
lookupScheme id ((id' :>: sc):as) = if id == id'
                            then return sc
                            else lookupScheme id as

-- Tracks the current substitution and unique TyGeneric
data Inference a
  = Inference (Substitution -> Int -> (Substitution,Int,a))

instance Monad Inference where
  return x            = Inference (\s n -> (s,n,x))
  (Inference f) >>= g = Inference (\s n -> let (s',n',x) = f s n
                                            in let Inference gx = g x
                                                in gx s' n')

runInference              :: Inference a -> a
runInference (Inference f) = let (s,n,x) = f nullSubstitution 0 in x

getSubstitution :: Inference Substitution
getSubstitution  = Inference (\s n -> (s,n,s))

extendSubstitution    :: Type -> Type -> Inference ()
extendSubstitution a b = do s <- getSubstitution
                            u <- unify (substitute s a) (substitute s b)
                            Inference (\s' n -> (u @@ s',n,()))

newVariable        :: Kind -> Inference Type
newVariable KiStack = Inference (\s n -> (s,n+1,(TyStack (StBottom ("v" ++ show n)))))
newVariable k       = Inference (\s n -> (s,n+1,(TyVariable ("v" ++ show n) k)))

freshVars              :: Scheme -> Inference (Qualified Type)
freshVars (ForAll ks t) = do ts <- mapM newVariable ks
                             Inference (\s n -> (s,n,instantiate ts t))

class CanInstantiate t where
  instantiate :: [Type] -> t -> t

instance CanInstantiate Type where
  instantiate ts (TyGeneric n)       = ts !! n
  instantiate ts (TyApplication i o) = TyApplication (instantiate ts i) (instantiate ts o)
  instantiate ts t                   = t

instance CanInstantiate a => CanInstantiate [a] where
  instantiate ts = map (instantiate ts)

instance CanInstantiate a => CanInstantiate (Qualified a) where
  instantiate ts (ps :=> t) = instantiate ts ps :=> instantiate ts t

instance CanInstantiate Predicate where
  instantiate ts (MemberOf id t) = MemberOf id (instantiate ts t)

type Infer e t
  = ClassEnv -> [Assumption] -> e -> Inference ([Predicate], t)

tiLiteral             :: Literal -> Inference ([Predicate], Type)
tiLiteral (LiString _) = return ([], tString)
tiLiteral (LiChar _)   = return ([], tChar)
tiLiteral (LiInt _)    = do v <- newVariable KiType
                            return ([MemberOf "Num" v], v)
tiLiteral (LiFloat _)  = do v <- newVariable KiType
                            return ([MemberOf "Fractional" v], v)

--tiTerm                      :: Infer Term Type
--tiTerm ce as (TmLiteral t)   = do (ps, t') <- tiTerm ce as t
--                                  return (ps, t')
--tiTerm ce as (TmCompose s t) = do
--tiTerm ce as (TmQuote t)     = do (ps, t') <- tiTerm ce as t
--                                  
--tiTerm ce as (TmName id)     = do sc <- lookupScheme id as
--                                  (ps :=> t) <- freshVars sc
--                                  return (ps, t)
--tiTerm ce as TmEmpty         = do i <- newVariable KiStack
--                                  return ([], tInt)

