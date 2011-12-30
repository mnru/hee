-- Catamorphism
data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving Show

-- Pair which maps a leaf value to a result value, or
-- combines two result values into a single result value
--   a is datum type
--   r is result type
type TreeAlgebra a r = (a -> r, r -> r -> r)

-- Applies a TreeAlgebra to a tree and yields it's result. This
-- is a catamorphism for the Tree datatype; tdepth and tsum are
-- called algebras
tfold :: TreeAlgebra a r -> Tree a -> r
tfold (f, g) (Leaf x)     = f x
tfold (f, g) (Branch l r) = g (tfold (f, g) l) (tfold (f, g) r)

tdepth :: TreeAlgebra a Integer
tdepth  = (\x -> 1, \l r -> 1 + max l r)

tsum :: (Num a) => TreeAlgebra a a
tsum  = (\x -> x, \l r -> l + r)
