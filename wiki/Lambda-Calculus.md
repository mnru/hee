
## Simply Typed Lambda Calculus

Term language

    e := x        variable
         e1 e2    application
         λx:τ.e   abstraction
         ...

Type language

    τ := α        type variable
         τ1 → τ2  arrow (function)
         ...

## Extentions

### Unit

Term language

    e := ...
         ()

Type language

    τ := ...
         unit

### Booleans

Term language

    e := ...
         true                     construct
         false                    construct
         if e1 then e2 else e3    deconstruct

Type language

    τ := ...
         bool

### Pairs

Term language

    e := ...
         (e1, e2)     construct
         project1 e   deconstruct
         project2 e   deconstruct

Type language

    τ := ...
         τ1 × τ2

### Sums

Term language

    e := ...
         inject1 e            construct
         inject2 e            construct
         case e1 of e2 ◇ e3   deconstruct

Type language

    τ := ...
         τ1 + τ2

### Recursive Functions

Term language

    e := ...
         μf:τ.λx.e

Unfolding recursive applications

    (μf:τ.λx.e1) e2 ---> [f ↦ μf:τ.λx.e1][x ↦ e2] e1

Types are not extended as they already describe functions. There is no
type-level distinction between recursive and non-recursive functions.

### Let and Letrec bindings

The binding constructs

    let x:τ = e1 in e2
    let rec (f:τ) x = e1 in e2

Can be viewed as syntactic sugar for

    (λx:τ.e2) e1
    let f = (μf:τ.λx.e2) in e1


