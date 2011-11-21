package com.github.kputnam.bee.types

import com.github.kputnam.bee.static._

case class ExistentialType(α : VariableLike, σ: Type) extends QuantifiedType {

  override def toString =
    "(∃" + α + "." + σ + ")"

  def freeVariables =
    σ.freeVariables - α

  def substitute(s: Substitution) =
    ExistentialType(α, σ.substitute(s \ α))

  override def instantiate(α: VariableLike, σ: Type) =
    if (this.α.id == α.id)
      this.σ.substitute(Substitution(α -> σ))
    else
      ExistentialType(this.α, this.σ.instantiate(α, σ))

}
