package com.github.kputnam.bee.types

case object ByteType extends MonomorphicType {
  override def toString = "byte"

  def unifyWith(t: AbstractType, s: Substitution) = t.substitute(s) match {
    case ByteType => Some(s)
    case t: TypeVariable => Some(s.addBinding(t, this))
    case _ => None
  }
}
