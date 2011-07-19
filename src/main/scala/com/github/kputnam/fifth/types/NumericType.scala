package com.github.kputnam.bcat.types

case object NumericType extends MonomorphicType {
  override def toString = "NumericType"
  def unifyWith(t: AbstractType, s: Substitution) = t.substitute(s) match {
    case NumericType => Some(s)
    case t: TypeVariable => Some(s.addBinding(t, this))
    case _ => None
  }
}
