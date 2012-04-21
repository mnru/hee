# Bumbling about Pointless Programming

> Kyle Putnam
> April 21, 2012

Point-free style is a paradigm in which function definitions do not include
information about its arguments. Instead, functions are defined in terms of
combinators and composition (Wikipedia).

## Syntax

Like many stack-based languages, Bee uses an postfix syntax for expression.
Operands are written before operators, e.g. `1 3 +`.

### Kinds

    κ,ι ::= @       -- stack
          | *       -- manifest
          | κ → ι   -- function

Kinds classify types. Bee distinguishes stack-types, value-types, and type
constructors from one another.

### Types

    σ,φ ::= ∅       -- empty
          | S,T     -- variable
          | σ τ     -- non-empty

    τ,υ ::= a,b     -- variable
          | σ → φ   -- function
          | int
          | str
          | ...

Currently the type system is too under-powered to be useful. Several features
like quantified types, qualified types, type constructors, and recursive types
are intended, once I understand them better.

### Terms

    e,f ::= ∅         -- empty
          | e f       -- composition
          | [e]       -- abstraction
          | name      -- 
          | literal   --

Unlike an applicative language, terms are built almost entirely by function
composition. For instance, `+ *` composes an addition with a multiplication.
Similarly, `1 +` composes a function `1` that pushes the numeric value one on
the stack with a function `+` that pops the top two stack elements and pushes
their sum.

## Minimal Combinators

These combinators are used to manipulate function values

    quote   : S a → S (T → T a)
    apply   : S (S → T) → T
    compose : S (T → U) (U → V) → S (T → V)
    dip     : S (S → T) a → T a

Because arguments aren't explicitly named, they must be accessed according to
their position on the stack. Several stack-shuffling combinators are provided
to move values around. For example:

    pop  : S a → S
    dup  : S a → S a a
    swap : S a b → S b a
    over : S a b → S a b a
    dig  : S a b c → S b c a

## Algebraic Data Types

Declaring an algebraic data type implicitly creates a deconstructor. This is a
function which takes function arguments corresponding to each constructor, in
the same order as the constructor definitions. For instance, `true ["T"] ["F"]
unboolean`. Note `if` is nothing more than `unboolean`.

    :: boolean
     | true
     | false
     ;

    -- unboolean : S boolean (S → T) (S → T) → T

Boolean operators can be implemented like so:

    : not [false] [true]    unboolean ;
    : and [id] [pop false]  unboolean ;
    : or  [pop true] [id]   unboolean ;

Like other languages with algebraic data types, each constructor can wrap any
fixed number of values. The current notation is inadequate for type checking,
and the field names are ignored -- this will change.

    :: list
     | null
     | cons tail head
     ;

    -- unlist : S a-list (S → T) (S a-list a → T) → T

## Exploiting Multiple Return Values

In non stack-based languages, functions must return at most one value. Multiple
values can be simulated by packaging them into one, and then unpacking them in
the caller.

    nextFree :: Int → [Int] → (Int, [Int])
    nextFree current []     = (current+1, [])
    nextFree current (v:vs) = if current+1 < v
                              then (current+1, v:vs)
                              else nextFree v vs

In a stack-based language, "functions" can return any number of values, including
zero, by pushing them onto the stack. The next-free function, given a sorted list
of bound variables and a starting point, shrinks the list of bound variables that
need to searched on the next call, and also returns the next free variable.

    : next-free               -- S num-list num → S num-list num
      swap                    -- id xs
        [1 + null swap]       --   id → null id'
        [dig 1 + 2dup >       --   id xs' x → xs' x id' → xs' x id' bool
            [[cons] dip]      --     xs' x id' → xs id'
            [pop next-free]   --     xs' x id' → ...
          if]
      unlist ;

This lets us call `next-free` like so:

    : bound-vars null 5 cons 4 cons 2 cons ;

    bound-vars 0      -- [2,4,5] 0
      next-free       -- [2,4,5] 1
      next-free       --   [4,5] 3
      next-free       --      [] 6
      next-free       --      [] 7

## Life Without (Implicit) Closures

Bee doesn't have nested scopes, mutable bindings, or parameter names so closures
as we know them aren't applicable. However, we can exploit other features of the
language to achieve similar results.

    : generate-free'    -- xs x
      next-free         -- xs' x'
      swap quote        -- x' [xs']
      over quote        -- x' [xs'] [x']
      [generate-free']  -- x' [xs'] [x'] [generate-free']
      compose compose ; -- x' [xs' x' generate-free']

Values can be lifted to functions using `quote`, and functions can be composed
with `compose`. This enables state encapsulation (immutable) within a function.

    : generate-free
      quote [-1 generate-free'] compose ;

These two features also enable partial application, e.g. `[-1 generate-free']`
and applying one argument from the top of the stack is `quote swap compose`.
Using this technique we have hidden the list of unbound variables from the
caller.

    bound-vars        -- [2,4,5]
      generate-free   -- [...]
      apply           -- 0 [...]
      apply           -- 0 1 [...]
      apply           -- 0 1 3 [...]
      apply           -- 0 1 3 6 [...]

Each time the function is applied, it produces the next free variable and also
produces the next *function* embedding any necessary state to generate the
remaining free variables.

## Typing Rules

Each rule corresponds to one of expressions forms


    T-EMPTY   -----------
               ∅ : S → S


               e : S → T    f : T → U
    T-COMPOSE ------------------------
                    e f : S → U


                    e : S → T
    T-QUOTE   ---------------------
               [e] : U → U (S → T)


                Γ(name) = S → T
    T-NAME    ------------------
                 name : S → T


    T-LITERAL ------------------------------------------------------------
               1 : S → S int    'a : S → S char    "x" : S → S str    ...


The type context Γ is elided from most rules for brevity, and also because
expressions cannot extend it during computation. That is, only definitions
(not expressions) can bind values to names.

### Example 

Consider the term `swap compose apply 1 +`.

              S       T                         T              U
            .---.   .---.              .---------------.   .-------.

     swap : A b c → A c b    compose : D (E → F) (F → G) → D (E → G)
    ----------------------------------------------------------------- T-COMPOSE
              swap compose : A (F → G) (E → F) → A (E → G)

We've unified `A c b` with `D (E → F) (F → G)`, which results in

    c = E → F
    b = F → G

Then we can apply that substitution to `S`, the input type for `swap`. We also
apply the substitution to `U`, the output type of `compose`.

Next, `swap compose apply`:

                            S               T                    T       U
                    .---------------.   .-------.            .-------.   -

     swap compose : A (F → G) (E → F) → A (E → G)    apply : H (H → I) → I
    ----------------------------------------------------------------------- T-COMPOSE
                  swap compose apply : A (F → G) (A → F) → G

Here we unify `A (E → G)` with `H (H → I)`, resulting in the substitution

    I = G
    H = E
    H = A
    E = A

This substitution is applied to `S`, the input type for `swap compose`, and `U`,
the output type of `apply`. Notice the constraints propogated *backward* to the
input, and now the function at the top of the stack must have a domain matching
`A`, the stack below the second item on the stack.

Lastly, `swap compose apply +`:

                                  S           T            T         U
                          .---------------.   -        .-------.   .---.

     swap compose apply : A (F → G) (A → F) → G    + : K int int → K int
    --------------------------------------------------------------------- T-COMPOSE
         swap compose apply + : A (F → K int int) (A → F) → K int

Like before, we unify `G` with `K int int`, resulting in the substitution:

    G = K int int

Then we apply this substitution to `A (F → G) (A → F) → K int`, because the
result of T-COMPOSE is `S → U`. Notice again that constraints propogated so
we've restricted the type of the original input by composing with another
term.

## Related Links

* [Stack machine](http://en.wikipedia.org/wiki/Stack_machine)
* [Stack-oriented programming language](http://en.wikipedia.org/Stack-oriented_programming_language)
* [concatenative.org](http://concatenative.org/)
* [My History with Forth Stack Machines](http://www.yosefk.com/blog/my-history-with-forth-stack-machines.html)
* [Understanding What It's Like to Program in Forth](http://prog21.dadgum.com/33.html)
* [BEES!](http://www.youtube.com/watch?v=5J2kc4oZTVU)
