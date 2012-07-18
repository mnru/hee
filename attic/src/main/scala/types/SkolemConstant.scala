package com.github.kputnam.bee
package types

import static._

case class SkolemizedTail(α: Tail) extends StackType with MonomorphicLike {
  override def asWord = throw new UnsupportedOperationException

  def top  = throw new NoSuchElementException("top of placeholder stack")
  def rest = throw new UnsupportedOperationException("rest of placeholder stack")
}

case class SkolemizedVariable(α: Variable) extends Type with MonomorphicLike {
  override def asWord = throw new UnsupportedOperationException

  def top  = throw new NoSuchElementException("top of placeholder stack")
  def rest = throw new UnsupportedOperationException("rest of placeholder stack")
}
