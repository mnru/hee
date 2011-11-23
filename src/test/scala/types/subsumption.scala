/**
 * Monotypes
 *   num        ≤ num
 *   num → bool ≤ num → bool
 *
 * Rank-1 polymorphism
 *   ∀α.α → α                     ≤ num → num
 *   ∀α.α → α                     ≤ ∀β.list(β) → list(β)
 *   ∀α.α → α                     ≤ ∀β.∀γ.pair(β,γ) → pair(γ,β)
 *   ∀α.∀β.pair(α,β) → pair(β,α)  ≤ ∀γ.pair(γ,γ) → pair(γ,γ)
 *
 * But this isn't ordinary unification, because
 *   num ≤ num                   !≤ ∀α.α → α
 *   ∀β.list(β) → list(β)        !≤ ∀α.α → α
 *   ∀β.∀γ.pair(β,γ) → pair(γ,β) !≤ ∀α.α → α
 *   ∀γ.pair(γ,γ) → pair(γ,γ)    !≤ ∀α.∀β.pair(α,β) → pair(β,α) 
 *
 * Perhaps we really need something like SkolemConstant(id) to
 * ensure skolem constants are distinct from type variables.
 */
