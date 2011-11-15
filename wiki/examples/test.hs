foo :: Int -> Int -> Int
foo = (+)

between :: (Ord a) => a -> a -> a -> Bool
between x y z = x <= y && y <= z

ordered :: (Ord a) => a -> a -> (a, a)
ordered x y = if x < y
              then (x, y)
              else (y, x)
