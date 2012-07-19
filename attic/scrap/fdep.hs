{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
data Vector = Vector Int Int
  deriving (Eq, Show)

data Matrix = Matrix Vector Vector
  deriving (Eq, Show)

class Mult a b c | a b -> c where
  (*) :: a -> b -> c
   
instance Mult Matrix Matrix Matrix where
  a * b = a

instance Mult Matrix Vector Vector where
  a * b = b

instance Mult Matrix Vector Char where
  a * b = 'c'
