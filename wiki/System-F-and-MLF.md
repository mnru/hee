I have been reading http://blog.huoc.org/r:in:mlf-story/, and learned that
System F, which has impredicative polymorphism, cannot assign a principle
type to all terms. The example given was setup with

    choose : ∀α. α → α → α
    choose = λx. λy. if true then x else y

    id : ∀α. α → α
    id = λx. x

Now the expression `choose id` would be assigned the type `∀β. (β → β) → β → β`
in plain Hindley-Milner, because we instantiate `choose` with the *monotype* of
`id: β → β`. With the impredicative polymorphism of System F, we can assign the
type `(∀β. β → β) → (∀β. β → β)`, as we can instantiate `choose` with a *polytype*
instead.

    σ₁ : ∀β. (β → β) → (β → β)
    σ₂ : (∀β. β → β) → (∀γ. γ → γ)

The equivalent System-F types are

    σ₁ : Λβ:*. (λx:β. x) → (λx:β. x)
    σ₂ : (Λβ:*. β → β) → (Λγ:*. γ → γ)

Spelling out the equivalent System-F types for both shows that `σ₁` is a *type
abstraction* that yields a term abstraction. The caller supplies a type parameter
named `β` and receives a term with type `(β → β) → (β → β)`. For example, the
caller might supply `int` for `β` and get back `(int → int) → (int → int)`. With
the knowledge `inc : int → int`, we can assert `choose id inc` is valid.

On the other hand, `σ₂` is a *term abstraction* that yields a type abstraction.
The caller supplies a type abstraction as an parameter (which must yield a term
abstraction when applied) and receives another type abstraction in return. This
time, `σ₂` can instantiate the given type abstraction with any type it chooses,
so we can describe terms like `f = λx. if (x true) and (x 0) > 0 then x else x`.
We can then assert `choose id f` is valid, however contrived it may be.

But if `choose id` has the type `σ₁`, then `choose id f` is *not* valid because
`f` has a subterm `x true` that violates the type `int → int` of `inc`. Similarly,
if `choose id` has the type `σ₂`, then `choose id inc` is not valid because `inc`
is not polymorphic. That is `σ₂` requires that `inc` be applicable to any type,
like we saw in `f`, but clearly `inc` may only be applied to `int` values.

Both `σ₁` and `σ₂` are admissable types, but System F hasn't a more general type
that subsumes both. 
