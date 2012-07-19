data Vector a = Vector a a a
                deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector a b c) (Vector x y z) = Vector (a + x) (b + y) (c + z)
