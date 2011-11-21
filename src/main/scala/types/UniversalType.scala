package com.github.kputnam.bee.types

import com.github.kputnam.bee.static._

/**
 * Universally qualified type
 *
 * Notes on subsumption of polymorphic types
 *                         num ≤ num
 *                  num → bool ≤ num → bool
 *                    ∀α.α → α ≤ num → num
 *                    ∀α.α → α ≤ ∀β.list(β) → list(β)
 *                    ∀α.α → α ≤ ∀β.∀γ.pair(β,γ) → pair(γ,β)
 * ∀α.∀β.pair(α,β) → pair(β,α) ≤ ∀γ.pair(γ,γ) → pair(γ,γ)
 */
case class UniversalType(α: VariableLike, σ: Type) extends QuantifiedType {

  override def toString =
    "(∀" + α + "." + σ + ")"

  def freeVariables =
    σ.freeVariables - α

  def substitute(s: Substitution) =
    UniversalType(α, σ.substitute(s \ α))

  override def instantiate(α: VariableLike, σ: Type) =
    if (this.α.id == α.id)
      this.σ.substitute(Substitution(α -> σ))
    else
      UniversalType(this.α, this.σ.instantiate(α, σ))

}
