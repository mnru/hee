package com.github.kputnam.bee.types

object TypeVariable {
  def fromString(c: Char) =
    new TypeVariable(Variable.toInt(c))

  def fromString(s: String) =
    new TypeVariable(Variable.toInt(s))
}

case class TypeVariable(id: Int) extends AbstractType with Variable {
  def alphabet =
    Variable.lowerGreek

  def substitute(s: Substitution) =
    s.getOrElse(this, this) match {
      case t: StackType => throw new UnsupportedOperationException(toString + " resolved to " + t)
      case t => t
    }
}
