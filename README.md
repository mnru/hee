# Bumbling about Pointless Programming

Point-free style is a paradigm in which function definitions do not include
information about its arguments. Instead, functions are defined in terms of
combinators and composition (Wikipedia). **Hee** is a concatenative, functional
programming language built for no practical purpose in mind -- my goal is to
use it as a vehicle for understanding PL topics. 

## Development Status [![Build Status](https://secure.travis-ci.org/kputnam/hee.png)](http://travis-ci.org/kputnam/hee)

**Hee** is in the very early stages of development. Some aspects of the syntax
haven't settled enough to allow providing examples. This includes the type
syntax, comments, function declarations, and type declarations. The general
goal is to keep the concrete syntax as minimal as possible, so I'm still
looking for ways to implement these features without adding "extra" syntax.

The preliminary type system is not usable. Several issues must be addressed
before certain simple terms can be correctly typed. Some terms that pose
interesting problems are:

* `dup` which seems to break concatenativity without impredicative polymorphism
* `dup apply`, the U-combinator, will probably require recursive types
* `dup compose`, because `A (B → B) → A (B → B)` is not sufficiently general
* The Y-combinator, because `A ((B → C) → (B → C)) → A (B → C)` is not correct,
  though I need to verify that.

### Current

I'm starting from a clean slate in Haskell. Prerequisites include ghc 7.4 and
haskell-platform 2012.

    $ git clone git://github.com/kputnam/hee.git
    $ cd hee
    $ cabal install

Printing parse the tree

    $ hee [1 +]
    TmQuote (TmCompose (TmLiteral (LiNumber 1)) (TmName "+"))

Running tests

    $ cabal configure --enable-tests
    $ cabal build
    $ cabal test

### Attic

There's a REPL written in Ruby, in [`attic/bin/bee.rb`](hee/blob/master/attic/bin/bee.rb).
This includes a few features like tab-completion, execution traces, and the
ability to save definitions created in the REPL to an external file. The
interpreter achieves tail-call optimization easily because it effectively
implements [subroutine threading](http://en.wikipedia.org/wiki/Threaded_code#Subroutine_threading).

There is a small runtime library in [`attic/runtime`](hee/blob/master/attic/runtime)
directory that is loaded when the REPL starts. Mostly this includes some type
definitions, like lists and booleans with a number of functions to operate on
these types. These files include what *appears* to be module declarations and
comments, however these are parsed as top-level expressions which are discarded
by the parser. The parser only reads *definitions* from files.

### Goals

Since my primary motivation for developing **hee** is to develop a deeper
theoretical and practical understanding of programming languages and type
systems. I am less concerned with developing a practically *usable* language.

For example, one motivation behind using postfix syntax is it is simple to
parse, though may be harder for humans to read and write. Using point-free
notation means the symbol table doesn't need to maintain information about
the current scope: there are no "local variables". These kinds of choices
simplify the language implementation, and may (or may not) yield benefits
for programmers using the language.

Some features I'd like to explore include:

* Module systems
* Type inference
* Quasiquotation
* Interactive development
* Comprehensible type errors

## Syntax

Like many stack-based languages, **hee** uses an postfix syntax for expression.
Operands are written before operators, e.g. `1 3 +`.

### Kinds

    κ,ι ::= @       -- stack
          | *       -- manifest
          | κ → ι   -- function

Kinds classify types. **Hee** distinguishes stack-types, value-types, and type
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
          | name      -- top-level definition
          | literal   -- char, num, str, etc

Unlike an applicative language, terms are built almost entirely by function
composition. For instance, `+ *` composes an addition with a multiplication.
Similarly, `1 +` composes a function `1` that pushes the numeric value one on
the stack with a function `+` that pops the top two stack elements and pushes
their sum.

## Minimal Combinators

These combinators are used to manipulate function values. Here are the type
signatures of some of these combinators:

    quote   : S a → S (T → T a)
    apply   : S (S → T) → T
    compose : S (T → U) (U → V) → S (T → V)
    dip     : S (S → T) a → T a

Because arguments aren't explicitly named, they must be accessed according to
their position on the stack. Several stack-shuffling combinators are provided
to move values around. Below are type signatures of some of these:

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

In most non stack-based languages, functions can return at most one value.
Multiple values can be simulated by packing them into one, and then unpacking
them in the callee.

    nextFree :: Int → [Int] → (Int, [Int])
    nextFree current []     = (current+1, [])
    nextFree current (v:vs) = if current+1 < v
                              then (current+1, v:vs)
                              else nextFree v vs

In a stack-based language, "functions" can return any number of values,
including zero, by pushing them onto the stack. For example, the `next-free`
function, given a sorted list of bound ids and a starting point, shrinks
the list of bound ids that need to searched on the next call, and also
returns the next free id.

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

    bound-vars -1     -- [2,4,5] -1
      next-free       -- [2,4,5] 0
      next-free       -- [2,4,5] 1
      next-free       --   [4,5] 3
      next-free       --      [] 6
      next-free       --      [] 7

## Life Without (Implicit) Closures

**Hee** doesn't have nested scopes, mutable bindings, or parameter names so
closures as we know them aren't meaningful. However, we can exploit other
features of the language to achieve similar results.

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

These two features also enable partial application, e.g. `[-1 generate-free']`.
Using this technique we have hidden the list of unbound ids from the caller.

    bound-vars        -- [2,4,5]
      generate-free   -- [...]
      apply           -- 0 [...]
      apply           -- 0 1 [...]
      apply           -- 0 1 3 [...]
      apply           -- 0 1 3 6 [...]

Each time the function is applied, it produces the next free variable and also
produces the next *function* which embeds any necessary state to generate the
remaining free ids.

## Typing Rules

Each rule corresponds to one of the syntactical forms for terms


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
expressions cannot extend it during computation. That is, only top-level
definitions (not expressions) can bind values to names.

### Example 

Consider the term `swap compose apply 1 +`. We'll perform type inference
on this expression by evaluating one type judgement at a time.

First, `swap`. This is viewed as a composition with the empty term, so we'll
use `T-COMPOSE` to infer the type of `∅ swap`.

      e : S → T       f :   T   →   U
          -   -           .---.   .---.

      ∅ : S → S    swap : A b c → A c b
     ---------------------------------- T-COMPOSE
           ∅ swap : A b c → A c b

                   '----'   '---'
              e f :  S    →   U

So the pre-conditions of `T-COMPOSE` are unified with the types of `∅` and
`swap`. That is, `S = S`, `T = S`, `T = A b c`, and `U = A c b`. We perform
unification on the two equations for `T`, which yields the substitution:

    S = A b c

We apply this substitution to the post-condition of `T-COMPOSE`, which is
`S → U`. This yields the type of `∅ swap : A b c → A c b`. This shows that
composition with the empty term `∅` is unsurprisingly trivial.

Now we compose `swap` with the term `compose`, which follows similar steps.

              S   →   T                         T        →     U
            .---.   .---.              .---------------.   .-------.

     swap : A b c → A c b    compose : D (E → F) (F → G) → D (E → G)
    ----------------------------------------------------------------- T-COMPOSE
              swap compose : A (F → G) (E → F) → A (E → G)


We've unified both equations for `T` from the premise of the `T-COMPOSE`
rule: `A c b` with `D (E → F) (F → G)`, resulting in the substitution:

    c = E → F
    b = F → G

Then we can apply that substitution to `S → T`, the conclusion of `T-COMPOSE`,
which gives us `swap compose : A (F → G) (E → F) → A (E → G)`.

Next, compose the term `swap compose` with the term `apply`:

                            S         →     T                    T     → U
                    .---------------.   .-------.            .-------.   -

     swap compose : A (F → G) (E → F) → A (E → G)    apply : H (H → I) → I
    ----------------------------------------------------------------------- T-COMPOSE
                  swap compose apply : A (F → G) (A → F) → G

Here we unify `A (E → G)` with `H (H → I)`, resulting in the substitution

    I = G
    H = E
    H = A
    E = A

This time, the constraints propogated *backward* to the input `S`. Now the
function at the top of the stack must have the domain `A`, matching the stack
below the second element. Previously, its domain was `E` which was unrelated
to `A`.

Lastly, we compose the term `swap compose apply` with the term `+`:

                                  S         → T            T     →   U
                          .---------------.   -        .-------.   .---.

     swap compose apply : A (F → G) (A → F) → G    + : K int int → K int
    --------------------------------------------------------------------- T-COMPOSE
         swap compose apply + : A (F → K int int) (A → F) → K int

Like before, we unify `G` with `K int int`, resulting in the substitution:

    G = K int int

Then we apply this substitution to `S → U`, which is `A (F → G) (A → F) → K int`
in this case. Notice again that constraints propogated so we've restricted the
type of the input merely by composing with another term.

## Related Links

* [The Joy Programming Language](http://www.latrobe.edu.au/phimvt/joy.html)
* [Stack machine](http://en.wikipedia.org/wiki/Stack_machine)
* [Stack-oriented programming language](http://en.wikipedia.org/Stack-oriented_programming_language)
* [My History with Forth Stack Machines](http://www.yosefk.com/blog/my-history-with-forth-stack-machines.html)
* [Understanding What It's Like to Program in Forth](http://prog21.dadgum.com/33.html)
* [concatenative.org](http://concatenative.org/)
