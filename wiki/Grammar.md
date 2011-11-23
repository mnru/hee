
  Types
    τ ::=
      α
      τ->τ    function types
      ∀α.τ    universal types
      ∃α.τ    existential types
      ∏α.τ    dependent types
      ∑α.τ

  Terms
    e ::=
      x
      λx:τ.e
      e e'
      Λα.e
      e[τ]

  Values
    v ::=
      λx:τ.e
      Λα.e

  Type Environment
    Δ ::=
      .
      Δ,α

  Term Environment
    Γ ::=
      .
      Γ,x:τ

  α β γ δ ε ζ η θ ι κ λ μ ν ξ ο π ρ ς σ τ υ φ χ ψ ω
  Α Β Γ Δ Ε Ζ Η Θ Ι Κ Λ Μ Ν Ξ Ο Π Ρ   Σ Τ Υ Φ Χ Ψ Ω

  ∀ ∃ ∉ ∈ ∋ ∌ ∘ ∙ ∧ ∨ ∩ ∪ ∴ → ×

  ≍ equivalent to
  ≭ not equivalent to

  ≔ colon equals
  ≕ equals colon

  ≘ corresponds to

  ≝ equal to by definition

  ≡ identical to
  ≢ not identical to

  ≤ less-than or equal to
  ≥ greater-than or equal to
    not less-than or equal to
    not greater-than or equal to
  ≬ between

  ≺ preceeds
  ≼ preceeds or equal to
  ≻ succeeds
  ≽ suceeeds or equal to

  ⊂ subset of
  ⊆ subset of or equal to
  ⊃ superset of
  ⊇ superset of or equal to

  ⊎ multiset union
  ⊕

  ⊤
  ⊥

  ⊦
  ⊢

