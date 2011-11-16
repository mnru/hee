package com.github.kputnam.bee.types

case class ExistentialType(α : VariableLike, τ: Type) extends Type {

  override
  def toString =
    "(∃" + α + "." + τ + ")"

  def freeVariables =
    τ.freeVariables - α

  def substitute(s: Substitution) =
    new UniversalType(α, τ.substitute(s \ α))

  def instantiate(τa: Type) =
    τ.substitute(Substitution(α -> τa))

}
