# Parametric Polymorphism

## Rank-1 types

## Rank-2 types

There are still terms that cannot be described by rank-1 polymorphic types. Notice
we can assign the type `Pair String Int` to the term `f = (pair (id "foo") (id 100))`.
If we want to abstract over `id`, we could write `g = λx.(pair (x "foo") (x 100))`.
Now the parameter `x` must be defined as a function on both `String` and `Int` types.

We could try to assign the polymorphic type `(a -> a) -> Pair String Int` to the
entire term `g`, where the parameter `x` has the type `a -> a`, but it is now too
general. This is because we expect `x` to be a term like `id = λx.x`, but a term
like `succ = λn.(+ 1 n)` also has the type `a -> a` when `a = Int`. In this case
the subterm `x "foo"` would  add `1` to `"foo"`, clearly a meaningless operation.
To prevent this, the type checker must reject either `x 100` or `x "foo"` because
they contradict one another.

But wait a minute -- why could we assign a type to `f = (pair (id "foo") (id 100))`
when the type of `id` is `a -> a`? Isn't that the same type as `x`? We'll get to
that after some prerequisite explanation.

With rank-2 polymorphic types, we can specify that `x` has the type `∀a. a -> a`
which means "`x` has the type `a -> a` for all possible `a`s". In other words,
`x` *must* be defined on *all* types, not just `Int` like the previous example.
We can now reject the term `g succ` because `succ` has the type `Int -> Int` and
is not compatible with `∀a. a -> a`. The term `g id`, however, fits, because `id`
has the type `a -> a`, which can be generalized to `∀a. a -> a`.

Okay, so back to the question about why we can assign a type to `f`. The answer
is subtle -- the type of `id` is in fact `∀a. a -> a`, because `a -> a` by itself
doesn't make sense. For simplicitly, we implicitly create a universal qualifier
around the entire type that binds any free type variables. So all along, we were
describing the difference between `∀a.(a -> a) -> Pair String Int` and
`(∀a. a -> a) -> Pair String Int`.

### Inference


    -- Normally 'twice' requires a function with type a -> a, specifically
    -- one parameter and one result, because it is evaluated like f(f(a)).
    --
    -- But in a stack-based language, functions can return multiple values
    -- back onto the same stack that is used to pass parameters. That means
    -- we could
    --
    [+] twice : int int int -> int
    [negate] twice : int -> int

* Binders
  * types:  ∀ ∃
  * values: λ ∏

# Ad-Hoc Polymorphism

* Nominal
  * Each instance has separate definition

* Structural

* Coercion
  * Implicit coercion
    * `2.0 + 3` (coerce both to Float)
    * `3 + 2.0` (coerce both to Float)
    > FloatToInt(f: Float): Int
    > IntToFloat(i: Int): Float


* Existential types
  > Describes all values (top type)
  > Top = ∃α.α

    class α Eq
      eq : α α Boolean Function2

    instance Int Eq
      eq : Int Int Boolean Function2
      eq ≡  int-eq

    instance (α Eq) => (α List) Eq
      eq : (α List) (α List) Boolean Function2
      eq ≡
      xs ys eq ≡ [ xs ys [eq] zip-with ]
                 [ false ]
                 xs length =
                 ys length =
                 and if

    -- (α Eq, β Eq) is a prerequisite
    instance (α Eq, β Eq) => (α β Pair) Eq
      eq : (α β Pair) (α β Pair) Boolean Function2
      eq ≡  ...
      x y eq ≡ x fst y fst =
               x snd y snd =
               and

    -- Eq is a prerequisite/superclass of Ord
    class α Eq => α Ord
      lt : α α Boolean Function2


    -- Dictionary passing style
    sort : ∀α.Ord α -> [α] -> [α]

    let x = [1 .. 10]
     in eq x (sort x)

    let x = [1 .. 10]
     in (tcEqList tcEqInt).eq x (sort (tcOrdList tcOrdInt) x)
