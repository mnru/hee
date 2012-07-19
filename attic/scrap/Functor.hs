class Functor f where
  -- Kind of 'f' is inferred as * => *
  fmap :: (a -> b) -> f a -> f b

-- Note f = Maybe
instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just a) = Just (f a)

-- Note f = []
instance Functor [] where
  fmap f []     = []
  fmap f (x:xs) = (f x):(fmap f xs)
