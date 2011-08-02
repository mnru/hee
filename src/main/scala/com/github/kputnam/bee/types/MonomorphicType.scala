package com.github.kputnam.bee.types

abstract class MonomorphicType extends AbstractType {
  override def hasOccurrence(t: Variable) = false
  override def isPolymorphic = false
  override def isMonomorphic = true
  override def asWord = WordType(Remainder(0),
                                 Remainder(0) :+ this)

  def freeVariables = Set.empty
  def substitute(s: Substitution) = this

  def unifyWith(t: AbstractType, s: Substitution) = t.substitute(s) match {
    case t: MonomorphicType if t == this => Some(s)
    case t: TypeVariable => Some(s.addBinding(t, this))
    case _ => None
  }
}
