package com.github.kputnam.bee.types

case class ExistentialType(α : VariableLike, τ: Type) extends QuantifiedType {

  override
  def toString =
    "(∃" + α + "." + τ + ")"

  def freeVariables =
    τ.freeVariables - α

  def substitute(s: Substitution) =
    new UniversalType(α, τ.substitute(s \ α))

  override
  def instantiate(σ: Type) =
    τ.substitute(Substitution(α -> σ))

}
