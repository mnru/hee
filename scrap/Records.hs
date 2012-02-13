-- Can't derive Eq because of function members
data Moo = Moo { render :: Int -> Moo
               , bender :: Char -> Char
               , fender :: String }

-- Recursive bindings, m is "self"
m = Moo { render = \n -> m
        , bender = \c -> c
        , fender = "string" }

-- Update a field
n = m { fender = "thing" }
