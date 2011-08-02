package com.github.kputnam.bee.types

object TypeVariable {
  def fromString(c: Char) =
    new TypeVariable(Variable.toInt(c))

  def fromString(s: String) =
    new TypeVariable(Variable.toInt(s))
}

case class TypeVariable(id: Int) extends AbstractType with Variable {
  def alphabet = Variable.lowerGreek

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
