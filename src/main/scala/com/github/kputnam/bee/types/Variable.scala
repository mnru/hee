package com.github.kputnam.bee.types

object Variable {
  val lowerGreek = "αβγδεζηθικλμνξοπρςστυφχψω"
  val upperGreek = "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"
  val lowerLatin = "abcdefghijklmnopqrstuvwxyz"
  val upperLatin = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def toInt(c: Char): Int =
    List(lowerGreek, upperGreek, lowerLatin, upperLatin).foldLeft(-1) ((k, alphabet) => if (k < 0) alphabet.indexOf(c) else k)

  def toInt(s: String): Int =
    List(lowerGreek, upperGreek, lowerLatin, upperLatin).foldLeft(-1) ((k, alphabet) => if (k < 0) alphabet.indexOf(s) else k)
}

trait Variable { self: AbstractType =>
  protected def alphabet: String

  def id: Int

  override def isMonomorphic = false
  override def isPolymorphic = true
  override def hasOccurrence(that: Variable) =
    this.id == that.id

  def freeVariables: Set[Variable] =
    Set(this)

  def substitute(s: Substitution) =
    s.getOrElse(this, this)

  def occursIn(that: AbstractType) =
    that.hasOccurrence(this)

  override def toString = {
    val remainder = id % alphabet.length
    val dividend  = id / alphabet.length
    alphabet(remainder) + ("'" * dividend)
  }

  override def equals(that: Any) = that match {
    case that: Variable => this.id == that.id
    case _ => false
  }
}
