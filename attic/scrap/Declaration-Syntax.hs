

-- Algebraic Data Type
data A a            -- unary type constructor
  = B               -- nullary val constructor B :: A a
  | C Int [Char]    -- binary val constructor C :: Int -> [Char] -> A a
  | D (a -> a)      -- unary val constructor D :: (a -> a) -> A a

-- Type synonym
type E = A Bool
  -- D not :: A Bool
  -- D not :: E

-- Record syntax implies the first parameter is F a
data F a = G { m :: a             -- m :: F a -> a
             , n :: Int           -- n :: F a -> Int
             , o :: [a] -> [F a]  -- o :: F a -> [a] -> [F a]
             }

-- Defines a constructor I implicitly from given deconstructor
newtype H a = I { j :: a -> String }
  -- I :: (a -> String) -> H a
  -- j :: H a -> a -> String

  -- Deconstruct a constructed val
  -- (j . I) == id

