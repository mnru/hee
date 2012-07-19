package com.github.kputnam.bee
package types

import static._

trait MonomorphicLike { self: Type =>
  import WordType._

  override def hasOccurrence(x: VariableLike) = false
  override def isPolymorphic = false
  override def isMonomorphic = true
  override def asWord = WordType(Tail(0),
                                 Tail(0) :+ self)

  def freeVariables: Set[VariableLike] =
    Set.empty

  def substitute(s: Substitution): this.type =
    self
}
