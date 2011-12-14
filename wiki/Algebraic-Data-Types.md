General algebraic data types are a recursive sum type of product types. The
Haskell data type

    data List a = Nil | Cons a (List a)

can be given the type `List = λα.μβ. Unit + (α × β)`, and the constructors
are `Nil = roll (inl ())` and `Cons x l = roll (inr Pair(x, l))`. Notice the
calls to `roll` are made implicit, so the programmer doesn't need to directly
deal with equi-recursive types. Presumably deconstruction by pattern matching
calls `unroll` implicitly.
