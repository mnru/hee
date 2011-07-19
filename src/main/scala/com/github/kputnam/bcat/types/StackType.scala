package com.github.kputnam.bcat.types

import annotation.tailrec

object StackType {
  def empty: StackType = new Empty
  def apply(elements: AbstractType*): StackType =
    elements.foldLeft(empty)((stack, e) => e :: stack)
}

sealed abstract class StackType extends AbstractType {
  def top: AbstractType
  def rest: StackType
  def ::(top: AbstractType): StackType = new ::(top, this)
}

case class Remainder(id: Int) extends StackType with Variable {
  def top = throw new NoSuchElementException("top of placeholder stack")
  def rest = throw new UnsupportedOperationException("rest of placeholder stack")

  def alphabet = upperGreek

  def unifyWith(t: AbstractType, s: Substitution) = substitute(s) match {
    case m: :: => m.unifyWith(t, s)
    case m: Empty => m.unifyWith(t, s)
    case m: Remainder => t.substitute(s) match {
      case t :: r => Some(s.addBinding(m, t :: r))
      case t: Empty => Some(s.addBinding(m, t))
      case t: Remainder => Some(s.addBinding(m, t))
      case _ => None
    }
    case _ => None
  }
}

class Empty extends StackType {
  def top = throw new NoSuchElementException("top of empty stack")
  def rest = throw new UnsupportedOperationException("rest of empty stack")
  override def toString = ""

  def hasOccurrence(t: Variable) = false
  def isMonomorphic = true
  def isPolymorphic = false

  def variables = Set.empty
  def substitute(s: Substitution): StackType = this

  def unifyWith(t: AbstractType, s: Substitution) = t.substitute(s) match {
    case t: Empty => Some(s)
    case r: Remainder => Some(s.addBinding(r, this))
    case _ => None
  }
}

case class ::(top: AbstractType, rest: StackType) extends StackType {
  override def toString = top.toString + " " + rest.toString

  def hasOccurrence(t: Variable) = top.hasOccurrence(t) || rest.hasOccurrence(t)
  def isMonomorphic = top.isMonomorphic || rest.isMonomorphic
  def isPolymorphic = top.isPolymorphic || rest.isPolymorphic

  def variables = top.variables ++ rest.variables
  def substitute(s: Substitution): StackType =
    top.substitute(s) :: rest.substitute(s).asInstanceOf[StackType]

  def unifyWith(t: AbstractType, s: Substitution) = substitute(s) match {
    case top :: rest => t.substitute(s) match {
      case t :: r =>
        top.unifyWith(t, s).flatMap(s => rest.unifyWith(r, s))
      case t: Remainder =>
        if ((top :: rest).hasOccurrence(t)) None
        else Some(s.addBinding(t, top :: rest))
      case _ =>
        None
    }
    case _ =>
      None
  }
}
