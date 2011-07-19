package com.github.kputnam.bcat.types

case class TypeVariable(id: Int) extends AbstractType with Variable {
  val alphabet = lowerLatin

  def unifyWith(t: AbstractType, s: Substitution) = substitute(s) match {
    case m: TypeVariable => t.substitute(s) match {
      case t: TypeVariable => Some(s.addBinding(m, t))
      case t: Remainder => None
      case t =>
        if (t.hasOccurrence(m)) None
        else Some(s.addBinding(m, t))
    }
    case m: Remainder => None
    case m => m.unifyWith(t, s)
  }
}
