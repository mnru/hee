package com.github.kputnam.bcat.types

case object CharacterType extends MonomorphicType {
  override def toString = "char"

  def unifyWith(t: AbstractType, s: Substitution) = t.substitute(s) match {
    case CharacterType => Some(s)
    case t: TypeVariable => Some(s.addBinding(t, this))
    case _ => None
  }
}
