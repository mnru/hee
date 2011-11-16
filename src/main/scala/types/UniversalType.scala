package com.github.kputnam.bee.types

/**
 * Universally qualified type
 */
case class UniversalType(α: VariableLike, τ: Type) extends Type {

  override
  def toString =
    "(∀" + α + "." + τ + ")"

  def freeVariables =
    τ.freeVariables - α

  def substitute(s: Substitution) =
    new UniversalType(α, τ.substitute(s \ α))

  /**
   *  Γ ⊢ e:τ    α ∉ FV(Γ)
   * ----------------------
   *      Γ ⊢ e:∀α.τ
   *
   * The premise of the generalization rule includes the proviso α ∉ FV(Γ).
   * The idea here is that the type judgement Γ ⊢ e:τ must hold without any
   * assumptions involving α; if so, then we can conclude that α could have
   * been any type σ, and the type judgement Γ ⊢ e:τ[σ/α] would also hold.
   */
  def generalize =
    new UniversalType(Variable.fromName('A'), this)

  /**
   *   Γ ⊢ e:∀α.τ
   * --------------
   *  Γ ⊢ e:τ[σ/α]
   * 
   * The notation τ[σ/α] refers to the safe substitution of the type σ
   * for the type variable α in τ. Here the binding operator ∀ binds the
   * type variable α in the same way that λx binds the variable x in value
   * terms, and the notions of scope, free and bound variables are the same.
   * In particular, we can α-convert (rename) type variables as necessary
   * to avoid capturing free type variables when performing substitutions.
   */
  def instantiate(σ: Type) =
    τ.substitute(Substitution(α -> σ))

}
