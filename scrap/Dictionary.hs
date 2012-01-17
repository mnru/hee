import GHC.Integer
import GHC.Float

{-
class HasEq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

class HasEq a => HasOrd a where
  (<)  :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>)  :: a -> a -> Bool
  (<=) :: a -> a -> Bool

instance HasEq Int where
  a `==` b = False
  a `==` b = False

instance HasOrd Int where
  a `<`  b = False
  a `>`  b = False
  a `<=` b = False
  a `>=` b = False
-}

-- class declaration for HasEq
data HasEqDict a = HasEqDict  (a -> a -> Bool) (a -> a -> Bool)

-- desugared class method signatures
(==) :: HasEqDict a -> a -> a -> Bool
(/=) :: HasEqDict a -> a -> a -> Bool

-- record accessors
(==) (HasEqDict eq ne) = eq
(/=) (HasEqDict eq ne) = ne

-- class declaration for HasOrd
data HasOrdDict a = HasOrdDict (HasEqDict a) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool) (a -> a -> Bool)

-- desugared class method signatures
(<)  :: HasOrdDict a -> a -> a -> Bool
(>)  :: HasOrdDict a -> a -> a -> Bool
(<=) :: HasOrdDict a -> a -> a -> Bool
(>=) :: HasOrdDict a -> a -> a -> Bool

-- record accessors
(<)  (HasOrdDict eq lt lte gt gte) = lt
(>)  (HasOrdDict eq lt lte gt gte) = gt
(<=) (HasOrdDict eq lt lte gt gte) = lte
(>=) (HasOrdDict eq lt lte gt gte) = gte

-- instance declarations for HasEq
hasEqInteger = HasEqDict eqInteger neqInteger
hasEqFloat   = HasEqDict eqFloat neFloat
hasEqDouble  = HasEqDict eqDouble neDouble

-- instance declarations for HasOrd
hasOrdInteger = HasOrdDict hasEqInteger ltInteger gtInteger (\a b -> (ltInteger a b) || ((Main.==) hasEqInteger a b)) (\a b -> (gtInteger a b) || ((Main.==) hasEqInteger a b))
hasOrdFloat   = HasOrdDict hasEqFloat   ltFloat   gtFloat   (\a b -> (ltFloat   a b) || ((Main.==) hasEqFloat   a b)) (\a b -> (gtFloat   a b) || ((Main.==) hasEqFloat   a b))
hasOrdDouble  = HasOrdDict hasEqDouble  ltDouble  gtDouble  (\a b -> (ltDouble  a b) || ((Main.==) hasEqDouble  a b)) (\a b -> (gtDouble  a b) || ((Main.==) hasEqDouble  a b))

-- explicit dictionary passing
main = if (Main.<) hasOrdInt 3.0 2.0
       then putStrLn "3.0  < 2.0"
       else putStrLn "3.0 >= 2.0"
