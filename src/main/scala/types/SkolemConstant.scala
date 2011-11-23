package com.github.kputnam.bee
package types

import static._

case class SkolemConstant(α: VariableLike) extends Type {

  override def hasOccurrence(x: VariableLike) = false
  override def isPolymorphic = false
  override def isMonomorphic = true

  override def quote  = throw new UnsupportedOperationException
  override def asWord = throw new UnsupportedOperationException
  override def skolemize = this

  def freeVariables = Set.empty

  def substitute(s: Substitution) = this

  override def equals(that: Any) = that match {
    case SkolemConstant(β) => α == β
    case _ => false
  }

}
