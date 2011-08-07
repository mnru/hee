package com.github.kputnam.bee.types

object Variable {
  def fromString(c: Char) =
    new Variable(VariableLike.toInt(c))

  def fromString(s: String) =
    new Variable(VariableLike.toInt(s))
}

case class Variable(id: Int) extends Type with VariableLike {
  def alphabet =
    VariableLike.lowerGreek

  def substitute(s: Substitution) =
    s.getOrElse(this, this) match {
      case t: StackType => throw new UnsupportedOperationException(toString + " resolved to " + t)
      case t => t
    }
}
