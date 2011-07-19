package com.github.kputnam.bcat.types

trait Variable { self: AbstractType =>

  val lowerGreek = "αβγδεζηθικλμνξοπρςστυφχψω"
  val upperGreek = "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"
  val lowerLatin = "abcdefghijklmnopqrstuvwxyz"
  val upperLatin = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def id: Int
  def alphabet: String

  def isMonomorphic = false
  def isPolymorphic = true

  def variables: Set[Variable] =
    Set(this)

  def substitute(s: Substitution) =
    s.getOrElse(this, this)

  def hasOccurrence(that: Variable) =
    this.id == that.id

  def occursIn(that: AbstractType) =
    that.hasOccurrence(this)

  override def toString = {
    val remainder = id % alphabet.length
    val dividend  = id / alphabet.length

    alphabet.slice(remainder, remainder + 1) + ("'" * dividend)
  }

  override def equals(that: Any) = that match {
    case that: Variable => this.id == that.id
    case _ => false
  }
}
