package com.github.kputnam.bcat.types

abstract class AbstractType {
  def hasOccurrence(t: Variable): Boolean
  def isPolymorphic: Boolean
  def isMonomorphic: Boolean
  def variables: Set[Variable]
  def substitute(s: Substitution): AbstractType
  def unifyWith(t: AbstractType, s: Substitution): Option[Substitution]
}
