module Hee.Checker
  where

import Hee.Terms
import Hee.Types
import Hee.Substitution
import Hee.Unification

-- Return (in-stack-type, out-stack-type) for a given function
inOut :: Type -> Either String (Stack, Stack)
inOut f =
  case f of
    (TyApplication (TyApplication tc (TyStack i)) (TyStack o)) ->
      if tc == tFunc
      then Right (i,o)
      else Left "compose only defined on (->) i o"
    _ -> Left "composed only defined on (->) i o"



checkTerm :: Term -> Either String Type

-- T-EMPTY
checkTerm (TmEmpty) =
  let s = StBottom 0
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
  let s = StBottom 0
   in do f <- checkTerm a
         let f' = substitute (freshVars (freeVars s) (freeVars f)) f
         return $ s `mkFunc` (StPush s f')

-- T-LITERAL
checkTerm (TmLiteral a) =
  let s  = StBottom 0
      a' = checkLit a
   in return $ s `mkFunc` (StPush s a')


-- Γ(name) = τ
lookupName :: String -> Either String Type

-- S → S
lookupName "id" =
  let s = StBottom 0
   in return $ s `mkFunc` s

-- S a → S
lookupName "pop" =
  let a = mkVar 0
      s = StBottom 0
   in return $ (StPush s a) `mkFunc` s

-- S a → S a a
lookupName "dup" =
  let a = mkVar 0
      s = StBottom 0
   in return $ (StPush s a) `mkFunc` (StPush (StPush s a) a)

-- S a b → S b a
lookupName "swap" =
  let s = StBottom 0
      a = mkVar 0
      b = mkVar 1
   in return $ (StPush (StPush s a) b) `mkFunc` (StPush (StPush s b) a)

-- S a → S (T → T a)
lookupName "quote" =
  let s = StBottom 0
      t = StBottom 1
      a = mkVar 0
      f = t `mkFunc` (StPush t a)
   in return $ (StPush s a) `mkFunc` (StPush s f)

-- S (S → T) → T
lookupName "apply" =
  let s = StBottom 0
      t = StBottom 1
      f = s `mkFunc` t
   in return $ (StPush s f) `mkFunc` t

-- S (T → U) (U → V) → S (T → V)
lookupName "compose" =
  let s  = StBottom 0
      t  = StBottom 1
      u  = StBottom 2
      v  = StBottom 3
      f  = t `mkFunc` u
      g  = u `mkFunc` v
      fg = t `mkFunc` v
   in return $ (StPush (StPush s f) g) `mkFunc` (StPush s fg)

-- S a b c → S b c a
lookupName "dig" =
  let s = StBottom 0
      a = mkVar 1
      b = mkVar 2
      c = mkVar 3
   in return $ (StPush (StPush (StPush s a) b) c) `mkFunc` (StPush (StPush (StPush s b) c) a)

-- S u (S → T) → T u
lookupName "dip" =
  let u = mkVar 0
      s = StBottom 0
      t = StBottom 1
      f = s `mkFunc` t
   in return $ (StPush (StPush s u) f) `mkFunc` (StPush t u)

-- S → S bool
lookupName "true" =
  let s = StBottom 0
   in return $ s `mkFunc` (StPush s tBool)

-- S → S bool
lookupName "false" =
  let s = StBottom 0
   in return $ s `mkFunc` (StPush s tBool)

-- S bool (S → T) (S → T) → T
lookupName "unboolean" =
  let s = StBottom 0
      t = StBottom 1
      f = s `mkFunc` t
   in return $ (StPush (StPush (StPush s tBool) f) f) `mkFunc` t

-- S bool (S → T) (S → T) → T
lookupName "if" =
  lookupName "unboolean"

-- S → S a-list
lookupName "null" =
  let s  = StBottom 0
      a  = mkVar 1
      as = mkList a
   in return $ s `mkFunc` (StPush s as)

-- S a-list a → S a-list
lookupName "cons" =
  let s  = StBottom 0
      a  = mkVar 1
      as = mkList a
   in return $ (StPush (StPush s as) a) `mkFunc` (StPush s as)

-- S a-list (S → T) (S a-list a → T) → T
lookupName "unlist" =
  let s  = StBottom 0
      t  = StBottom 1
      a  = mkVar 2
      as = mkList a
      f  = s `mkFunc` t
      g  = (StPush (StPush s as) a) `mkFunc` t
   in return $ (StPush (StPush (StPush s as) f) g) `mkFunc` t

-- S a b → S a b a b
lookupName "2dup" =
  let s = StBottom 0
      a = mkVar 1
      b = mkVar 2
   in return $ (StPush (StPush s a) b) `mkFunc` (StPush (StPush (StPush (StPush s a) b) a) b)

-- S int int → S int
lookupName op
  | op `elem` ["+","*","-","/","^","%"] =
  let s = StBottom 0
      a = tInt
   in return $ (StPush (StPush s a) a) `mkFunc` (StPush s a)

-- S int int → S bool
lookupName op
  | op `elem` ["<","<=","==","!=",">=",">"] =
  let s = StBottom 0
      a = tInt
   in return $ (StPush (StPush s a) a) `mkFunc` (StPush s a)

lookupName x =
  Left $ "unbound identifier: '" ++ x ++ "'"


-- T-LITERAL
checkLit :: Literal -> Type
checkLit (LiInt _)    = tInt
checkLit (LiRatn _)   = tRatn
checkLit (LiChar _)   = tChar
checkLit (LiString _) = tString
