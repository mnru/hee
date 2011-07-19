package com.github.kputnam.bcat.types

case object BooleanType extends MonomorphicType {
  override def toString = "BooleanType"
  def unifyWith(t: AbstractType, s: Substitution) = t.substitute(s) match {
    case BooleanType => Some(s)
    case t: TypeVariable => Some(s.addBinding(t, this))
    case _ => None
  }
}
