import Data.List (nub, intersect, union, sort, foldl')

type Id
  = Int

type Variable
  = (Id,Kind)

type Substitution
  = [(Variable, Type)]

---------------------------------------------------------------------

data Type
  = TVariable Id Kind
  | TConstructor String Kind
  | TApplication Type Type
  | TForall Id Kind [Predicate] Type
  | TStack Stack
  deriving (Eq)

data Predicate
  = MemberOf Type
  deriving (Eq)

data Stack
  = SEmpty
  | SBottom Id
  | SPush Stack Type
  deriving (Eq)

data Kind
  = KStack
  | KType
  | KConstructor Kind Kind
  deriving (Eq)

---------------------------------------------------------------------

class HasKind t where
  kind :: t -> Kind

instance HasKind Kind where
  kind k = k

instance HasKind Stack where
  kind t = KStack

instance HasKind Type where
  kind (TVariable _ k)    = k
  kind (TConstructor _ k) = k
  kind (TStack _)         = KStack
  kind (TApplication i _) = let (KConstructor _ k) = kind i in k

instance (HasKind b) => HasKind (a,b) where
  kind (_,k) = kind k

---------------------------------------------------------------------

class CanUnify t where
  match   :: Monad m => t -> t -> m Substitution
  unify   :: Monad m => t -> t -> m Substitution
  bindvar :: Monad m => Variable -> t -> m Substitution

instance CanUnify Type where
  match (TApplication i o) (TApplication i' o')
                               = do a <- match i i'
                                    b <- match (substitute a o) (substitute a o')
                                    merge a b
  match (TConstructor id k) (TConstructor id' k')
    | id == id' && k == k'     = return empty
  match (TStack s) (TStack s') = match s s'
  match (TVariable id k) t     = bindvar (id,k) t
  match _ _                    = fail "merge failed"

  unify (TStack s) (TStack s') = unify s s'
  unify (TVariable id k) t     = bindvar (id,k) t
  unify t (TVariable id k)     = bindvar (id,k) t
  unify (TConstructor id k) (TConstructor id' k')
    | id == id' && k == k'     = return empty
  unify (TApplication i o) (TApplication i' o')
                               = do a <- unify i i'
                                    b <- unify (substitute a o) (substitute a o')
                                    return (a @@ b)
  unify _ _                    = fail "unify failed"

  bindvar v@(id,k) t
    | t == TVariable id k = return empty
    | v `elem` freeVars t = fail "bindvar failed (occurs check)"
    | k /= kind t         = fail "bindvar failed (kind mismatch)"
    | otherwise           = return (v +-> t)

instance CanUnify Stack where
  match (SBottom id) t = bindvar (id,KStack) t
  match SEmpty SEmpty  = return empty
  match (SPush t s) (SPush t' s')
                       = do a <- match t t'
                            b <- match (substitute a s) (substitute a s')
                            merge a b

  unify (SBottom id) t = bindvar (id,KStack) t
  unify t (SBottom id) = bindvar (id,KStack) t
  unify SEmpty SEmpty  = return empty
  unify (SPush t s) (SPush t' s')
                       = do a <- unify t t'
                            b <- unify (substitute a s) (substitute a s')
                            return (a @@ b)
  unify _ _            = fail "unify failed"

  bindvar v@(id,KStack) t = return (v +-> TStack t)
  bindvar _ _             = fail "bindvar failed (kind mismatch)"

---------------------------------------------------------------------

class CanSubstitute t where
  substitute :: Substitution -> t -> t
  freeVars   :: t -> [Variable]

instance CanSubstitute a => CanSubstitute [a] where
  substitute s = map (substitute s)
  freeVars     = nub . concat . map freeVars

instance CanSubstitute Type where
  substitute s (TApplication i o) = TApplication (substitute s i) (substitute s o)
  substitute s (TStack t)         = TStack (substitute s t)
  substitute s (TVariable id k)   = case lookup (id,k) s of
                                      Just t  -> t
                                      Nothing -> TVariable id k
  substitute s t = t

  freeVars (TApplication i o) = freeVars i `union` freeVars o
  freeVars (TStack t)         = freeVars t
  freeVars (TVariable id k)   = [(id,k)]
  freeVars _                  = []

instance CanSubstitute Stack where
  substitute s (SPush t h)  = SPush (substitute s t) (substitute s h)
  substitute s (SBottom id) = case lookup (id,KStack) s of
                                 Just (TStack t) -> t
                                 Just t  -> SBottom id
                                 Nothing -> SBottom id
  substitute s t = t

  freeVars (SBottom id) = [(id,KStack)]
  freeVars (SPush t h)  = freeVars t `union` freeVars h
  freeVars _             = []

---------------------------------------------------------------------

-- Composition of substitutions
merge :: Monad m => Substitution -> Substitution -> m Substitution
merge a b = if all match (map fst a `intersect` map fst b)
            then return (a ++ b)
            else fail "merge failed"
  where match (id,KStack) = substitute a (SBottom id) == substitute b (SBottom id)
        match (id,k)      = substitute a (TVariable id k) == substitute b (TVariable id k)

-- Composition of substitutions
--   substitute (a @@ b) = substitute a . substitute b
infixr 4 @@
(@@) :: Substitution -> Substitution -> Substitution
(@@) a b = [(v, substitute a t) | (v, t) <- b] ++ a

-- Empty subtitution
empty :: Substitution
empty = []

-- Singleton substitution
--   kind preserving iff (kind v) == (kind t)
(+->) :: Variable -> Type -> Substitution
(+->) v t = [(v, t)]

---------------------------------------------------------------------
