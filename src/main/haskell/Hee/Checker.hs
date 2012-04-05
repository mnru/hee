module Hee.Checker
  where

import Hee.Terms
import Hee.Types

checkTerm :: Stack -> Term -> Stack
checkTerm s (TmEmpty)       = undefined
checkTerm s (TmName id)     = undefined
checkTerm s (TmCompose a b) = undefined
checkTerm s (TmQuote a)     = undefined
checkTerm s (TmLiteral x)   = undefined

lookupName "id"  =
  mkFunc (StBottom "a") (StBottom "a")

-- S a -> S
lookupName "pop" =
  let a = mkVar "a"
      s = StBottom "S"
   in mkFunc (StPush s a) s

-- S a -> S a a
lookupName "dup" =
  let a = mkVar "a"
      s = StBottom "S"
   in mkFunc (StPush s a) (StPush (StPush s a) a)

-- S u (S -> T) -> T u
lookupName "dip" =
  let u = mkVar "u"
      s = StBottom "S"
      t = StBottom "T"
      f = mkFunc s t
   in mkFunc (StPush (StPush s u) f) (StPush t u)

-- S a b -> S b a
lookupName "swap" =
  let s = StBottom "S"
      a = mkVar "a"
      b = mkVar "b"
   in mkFunc (StPush (StPush s a) b) (StPush (StPush s b) a)

-- S a -> S (T -> T a)
lookupName "quote" =
  let s = StBottom "S"
      t = StBottom "T"
      a = mkVar "a"
      f = mkFunc t (StPush t a)
   in mkFunc (StPush s a) (StPush s f)

-- S (S -> T) -> T
lookupName "apply" =
  let s = StBottom "S"
      t = StBottom "T"
      f = mkFunc s t
   in mkFunc (StPush s f) t

-- S (T -> U) (U -> V) -> S (T -> V)
lookupName "compose" =
  let s  = StBottom "S"
      t  = StBottom "T"
      u  = StBottom "U"
      v  = StBottom "V"
      f  = mkFunc t u
      g  = mkFunc u v
      fg = mkFunc t v
   in mkFunc (StPush (StPush s f) g) (StPush s fg)

checkLit :: Literal -> Type
checkLit (LiInt _)    = tInt
checkLit (LiRatn _)   = tRatn
checkLit (LiChar _)   = tChar
checkLit (LiString _) = tString
