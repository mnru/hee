package com.github.kputnam.bee.types

abstract class MonomorphicType extends AbstractType {
  override def hasOccurrence(t: Variable) = false
  override def isPolymorphic = false
  override def isMonomorphic = true
  override def asWord = WordType(Remainder(0),
                                 Remainder(0) :+ this)

  def freeVariables = Set.empty

  def substitute(s: Substitution) = this
}
