package com.github.kputnam.bcat.types

case object StringType extends MonomorphicType {
  override def toString = "str"

  def unifyWith(t: AbstractType, s: Substitution) = t.substitute(s) match {
    case StringType => Some(s)
    case t: TypeVariable => Some(s.addBinding(t, this))
    case _ => None
  }
}
