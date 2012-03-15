module Hee.Classes
  (
  ) where

-- t ∈ Eq
-- t ∈ Num
-- t ∈ Collection(s)
data Predicate
  = MemberOf Id Type
  deriving (Eq, Show)

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
entail ce ps h = if any (h `elem`) (map (consequents ce) ps)
                 then True
                 else case antecedents ce h of
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
lookupScheme                     :: Monad m => Id -> [Assumption] -> m Scheme
lookupScheme id []                = fail "unbound identifier"
lookupScheme id ((id' :>: sc):as) = if id == id'
                            then return sc
                            else lookupScheme id as

-- Tracks the current substitution and TyGeneric counter
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
tiLiteral (LiInt _)    = newVariable KiType >>= \v -> return ([MemberOf "Num" v], v)
tiLiteral (LiFloat _)  = newVariable KiType >>= \v -> return ([MemberOf "Fractional" v], v)

tiTerm                      :: Infer Term Type
tiTerm ce as TmEmpty         = do (TyStack t) <- newVariable KiStack
                                  return ([], mkFunc t t)
tiTerm ce as (TmLiteral t)   = do x <- tiLiteral t
                                  return x
tiTerm ce as (TmName id)     = do sc <- lookupScheme id as
                                  (ps :=> h) <- freshVars sc
                                  return (ps, h)
--Term ce as (TmQuote t)     =
--Term ce as (TmCompose s t) =


