instance Monad ((->) i) where
  f >>= g  = (\i -> g (f i) i)
  return x = (\i -> x)

main = putStrLn $ (("Haske" ++) >>= (++)) "l"
