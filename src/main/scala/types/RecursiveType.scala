package com.github.kputnam.bee.types

// fix λx. e
// μα.τ

case class RecursiveType(α: Variable, τ: Type) extends Type {

  override
  def toString =
    "μ" + α + "." + τ

  override
  def unfold: Type =
    Substitution(α -> this)(τ)

  def freeVariables =
    τ.freeVariables - α

  def substitute(s: Substitution) =
    new RecursiveType(α, τ.substitute(s \ α))

}
