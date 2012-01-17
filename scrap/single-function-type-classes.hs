class Multiply a where
  (*) :: a -> a -> a

class Add a where
  (+) :: a -> a -> a

class Zero a where
  zero :: a

instance Multiply Int where
  (*) a b = (Prelude.*) a b

instance Add Int where
  (+) a b = (Prelude.+) a b

instance Zero Int where
  zero = 0

instance Multiply Char where
  (*) a b = min a b

instance Add Char where
  (+) a b = max a b

instance Zero Char where
  zero = 'a'

sumOfSq :: (Zero a, Multiply a, Add a) => [a] -> a
sumOfSq []     = zero
sumOfSq (x:xs) = (Main.+) ((Main.*) x x) (sumOfSq xs)
