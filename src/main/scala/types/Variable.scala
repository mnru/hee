package com.github.kputnam.bee
package types

import static._

object Variable {
  def fromName(c: Char) =
    Variable(VariableLike.toInt(c))

  def fromName(s: String) =
    Variable(VariableLike.toInt(s))
}

case class Variable(id: Int) extends Type with VariableLike {

  def alphabet =
    VariableLike.lowerGreek

  def substitute(s: Substitution) = s.getOrElse(this, this) match {
    case t: StackType => throw new UnsupportedOperationException(toString + " resolved to " + t)
    case t => t
  }

  override def skolemize =
    SkolemizedVariable(this)
}
