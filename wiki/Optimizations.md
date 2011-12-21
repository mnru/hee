## Predicate methods defined in terms of pattern matching functions

    data List a = Null
                | Cons a (List a)

    null? = [true]  [pop pop false] unList
    cons? = [false] [pop pop true]  unList

With two variants, we only need a 1-bit tag to distinguish Null
from Cons. So the optimimum implementation of `null?` and `cons?`
is just a bit operation. However, the compiler might not produce
optimal code when `null?` and `cons?` are defined in terms of the
pattern-matching destructor `unList`.

## Non-Strict Evaluation
With a static type system, the compiler knows the arity of each function and 
can identify subexpressions, change the order of their evaluation, or perhaps
do non-strict evaluation.

