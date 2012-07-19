module Hee.Checker
  where

import Hee.Terms
import Hee.Types
import Hee.Substitution
import Hee.Unification


-- Primitive types
tInt    = TConstructor "int"    KType
tRatn   = TConstructor "ratn"   KType
tChar   = TConstructor "char"   KType
tBool   = TConstructor "bool"   KType
tString = TConstructor "string" KType

-- Composite types
tPair   = TConstructor "(,)"  (KConstructor KType (KConstructor KType KType))
tFunc   = TConstructor "(->)" (KConstructor KStack (KConstructor KStack KType))
tList   = TConstructor "[]"   (KConstructor KType KType)

mkFunc :: Stack -> Stack -> Type
mkFunc inp out = TApplication (TApplication tFunc (TStack inp)) (TStack out)

mkList :: Type -> Type
mkList t = TApplication tList t

mkPair :: Type -> Type -> Type
mkPair fst snd = TApplication (TApplication tPair fst) snd

-- Return (in-stack-type, out-stack-type) for a given function
inOut :: Type -> Either String (Stack, Stack)
inOut f =
  case f of
    (TApplication (TApplication tc (TStack i)) (TStack o)) ->
      if tc == tFunc
      then Right (i,o)
      else Left "compose only defined on (->) i o"
    _ -> Left "composed only defined on (->) i o"



checkTerm :: Term -> Either String Type

-- T-EMPTY
checkTerm (TmEmpty) =
  let s = SBottom 0
   in return $ s `mkFunc` s

-- T-NAME
checkTerm (TmName id) =
  lookupName id

-- T-COMPOSE
checkTerm (TmCompose a b) =
  do ta  <- checkTerm a
     tb  <- checkTerm b
     let tb' = substitute (freshVars (freeVars ta) (freeVars tb)) tb
     (ai, ao) <- inOut ta
     (bi, bo) <- inOut tb'
     s <- unify ao bi
     return $ substitute s (ai `mkFunc` bo)

-- T-QUOTE
checkTerm (TmQuote a) =
  let s = SBottom 0
   in do f <- checkTerm a
         let f' = substitute (freshVars (freeVars s) (freeVars f)) f
         return $ s `mkFunc` (SPush s f')

-- T-LITERAL
checkTerm (TmLiteral a) =
  let s  = SBottom 0
      a' = checkLit a
   in return $ s `mkFunc` (SPush s a')


-- Γ(name) = τ
lookupName :: String -> Either String Type

-- S → S
lookupName "id" =
  let s = SBottom 0
   in return . generalize $ s `mkFunc` s

-- S a → S
lookupName "pop" =
  let a = mkVar 0
      s = SBottom 0
   in return . generalize $ (SPush s a) `mkFunc` s

-- S a → S a a
lookupName "dup" =
  let a = mkVar 0
      s = SBottom 0
   in return . generalize $ (SPush s a) `mkFunc` (SPush (SPush s a) a)

-- S a b → S b a
lookupName "swap" =
  let s = SBottom 0
      a = mkVar 0
      b = mkVar 1
   in return . generalize $ (SPush (SPush s a) b) `mkFunc` (SPush (SPush s b) a)

-- S a → S (T → T a)
lookupName "quote" =
  let s = SBottom 0
      t = SBottom 1
      a = mkVar 0
      f = t `mkFunc` (SPush t a)
   in return . generalize $ (SPush s a) `mkFunc` (SPush s f)

-- S (S → T) → T
lookupName "apply" =
  let s = SBottom 0
      t = SBottom 1
      f = s `mkFunc` t
   in return . generalize $ (SPush s f) `mkFunc` t

-- S (T → U) (U → V) → S (T → V)
lookupName "compose" =
  let s  = SBottom 0
      t  = SBottom 1
      u  = SBottom 2
      v  = SBottom 3
      f  = t `mkFunc` u
      g  = u `mkFunc` v
      fg = t `mkFunc` v
   in return . generalize $ (SPush (SPush s f) g) `mkFunc` (SPush s fg)

-- S a b c → S b c a
lookupName "dig" =
  let s = SBottom 0
      a = mkVar 1
      b = mkVar 2
      c = mkVar 3
   in return . generalize $ (SPush (SPush (SPush s a) b) c) `mkFunc` (SPush (SPush (SPush s b) c) a)

-- S u (S → T) → T u
lookupName "dip" =
  let u = mkVar 0
      s = SBottom 0
      t = SBottom 1
      f = s `mkFunc` t
   in return . generalize $ (SPush (SPush s u) f) `mkFunc` (SPush t u)

-- S → S bool
lookupName "true" =
  let s = SBottom 0
   in return . generalize $ s `mkFunc` (SPush s tBool)

-- S → S bool
lookupName "false" =
  let s = SBottom 0
   in return . generalize $ s `mkFunc` (SPush s tBool)

-- S bool (S → T) (S → T) → T
lookupName "unboolean" =
  let s = SBottom 0
      t = SBottom 1
      f = s `mkFunc` t
   in return . generalize $ (SPush (SPush (SPush s tBool) f) f) `mkFunc` t

-- S bool (S → T) (S → T) → T
lookupName "if" =
  lookupName "unboolean"

-- S → S a-list
lookupName "null" =
  let s  = SBottom 0
      a  = mkVar 1
      as = mkList a
   in return . generalize $ s `mkFunc` (SPush s as)

-- S a-list a → S a-list
lookupName "cons" =
  let s  = SBottom 0
      a  = mkVar 1
      as = mkList a
   in return . generalize $ (SPush (SPush s as) a) `mkFunc` (SPush s as)

-- S a-list (S → T) (S a-list a → T) → T
lookupName "unlist" =
  let s  = SBottom 0
      t  = SBottom 1
      a  = mkVar 2
      as = mkList a
      f  = s `mkFunc` t
      g  = (SPush (SPush s as) a) `mkFunc` t
   in return . generalize $ (SPush (SPush (SPush s as) f) g) `mkFunc` t

-- S a b → S a b a b
lookupName "2dup" =
  let s = SBottom 0
      a = mkVar 1
      b = mkVar 2
   in return . generalize $ (SPush (SPush s a) b) `mkFunc` (SPush (SPush (SPush (SPush s a) b) a) b)

-- S int int → S int
lookupName op
  | op `elem` ["+","*","-","/","^","%"] =
  let s = SBottom 0
      a = tInt
   in return . generalize $ (SPush (SPush s a) a) `mkFunc` (SPush s a)

-- S int int → S bool
lookupName op
  | op `elem` ["<","<=","==","!=",">=",">"] =
  let s = SBottom 0
      a = tInt
   in return . generalize $ (SPush (SPush s a) a) `mkFunc` (SPush s a)

lookupName x =
  Left $ "unbound identifier: '" ++ x ++ "'"


-- T-LITERAL
checkLit :: Literal -> Type
checkLit (LInt _)    = tInt
checkLit (LRatn _)   = tRatn
checkLit (LChar _)   = tChar
checkLit (LString _) = tString

normalizeVars :: [Variable] -> Substitution t
normalizeVars = freshVars []

normalizeType :: (CanUnify a, CanSubstitute a) => a -> a
normalizeType t = substitute (normalizeVars $ freeVars t) t

-- Returns a substitution that renames all variables in gs such that
--   freeVars fs `intersect` freeVars gs == []
freshVars :: [Variable] -> [Variable] -> Substitution
freshVars fs xs = thd types ++ thd stacks
  where
    thd (a,b,c) = c
    (tbound,sbound) = splitIds fs

    types :: (Id, [Id], Substitution)
    types  = foldl' (\(current, bound, sub) var ->
                      case var of
                        x@(_,KType) ->
                          let (current', bound') = nextFree current bound
                           in (current', bound', (x, TVariable current' KType):sub)
                        _ -> (current, bound, sub))
                    (-1, tbound, empty) xs

    stacks :: (Id, [Id], Substitution)
    stacks = foldl' (\(current, bound, sub) var ->
                      case var of
                        x@(_,KStack) ->
                          let (current', bound') = nextFree current bound
                           in (current', bound', (x, TStack $ SBottom current'):sub)
                        _ -> (current, bound, sub))
                    (-1, sbound, empty) xs

-- Split variables into list of KType ids and KStack ids
splitIds :: [Variable] -> ([Id], [Id])
splitIds vars = (sort ts, sort ss)
  where (ts,ss) = foldl' (\(ts, ss) x ->
                    case x of
                      (id,KType)  -> (id:ts, ss)
                      (id,KStack) -> (ts, id:ss)
                      _             -> (ts, ss)) ([], []) vars

nextFree :: Id -> [Id] -> (Id, [Id])
nextFree current []     = (current+1, [])
nextFree current (v:vs) = if current+1 < v
                          then (current+1, v:vs)
                          else nextFree v vs

generalize :: Type -> Type
generalize t = foldl' (\t (id,k) -> TForall id k [] t) t $ freeVars t
