package com.github.kputnam.bcat.types

import annotation.tailrec

object StackType {
  def empty: StackType = Empty

  def variable(id: Int): StackType = Remainder(id)

  // StackType(Z Y X ... C B A).top = A
  def apply(elements: AbstractType*): StackType =
    elements.foldLeft(empty)((stack, e) => stack :+ e)

  // StackType(List(Z Y X ... C B A)).top = A
  def apply(elements: List[AbstractType]): StackType =
    elements.foldLeft(empty)((stack, e) => stack :+ e)
}

sealed abstract class StackType extends AbstractType {
  def top: AbstractType
  def rest: StackType
  def ::(top: AbstractType): StackType = new NonEmpty(top, this)
  def :+(top: AbstractType): StackType = new NonEmpty(top, this)

  override
  def asWord = throw new UnsupportedOperationException
  def toWord = throw new UnsupportedOperationException
}

case class Remainder(id: Int) extends StackType with Variable {
  def top = throw new NoSuchElementException("top of placeholder stack")
  def rest = throw new UnsupportedOperationException("rest of placeholder stack")

  def alphabet = upperGreek

  def unifyWith(t: AbstractType, s: Substitution) = substitute(s) match {
    case Empty => Empty.unifyWith(t, s)
    case m: NonEmpty => m.unifyWith(t, s)
    case m: Remainder => t.substitute(s) match {
      case Empty => Some(s.addBinding(m, Empty))
      case t: NonEmpty => Some(s.addBinding(m, t))
      case t: Remainder => Some(s.addBinding(m, t))
      case _ => None
    }
    case _ => None
  }
}

case object Empty extends StackType {
  override def toString = "∅"

  def top = throw new NoSuchElementException("top of empty stack")
  def rest = throw new UnsupportedOperationException("rest of empty stack")

  override def ::(top: AbstractType): StackType = top match {
    case t: Remainder => t
    case t => new NonEmpty(top, this)
  }

  override def :+(top: AbstractType): StackType = top match {
    case t: Remainder => t
    case t => new NonEmpty(top, this)
  }

  def hasOccurrence(t: Variable) = false
  def isMonomorphic = true
  def isPolymorphic = false

  def variables = Set.empty
  def substitute(s: Substitution): StackType = this

  def unifyWith(t: AbstractType, s: Substitution) = t.substitute(s) match {
    case Empty => Some(s)
    case r: Remainder => Some(s.addBinding(r, this))
    case _ => None
  }
}

object :: {
  def apply(top: AbstractType, rest: StackType) =
    new NonEmpty(top, rest)

  def unapply(s: StackType): Option[(AbstractType, StackType)] = s match {
    case s: NonEmpty => Some((s.top, s.rest))
    case _ => None
  }
}

object :+ {
  def apply(rest: StackType, top: AbstractType) =
    new NonEmpty(top, rest)

  def unapply(s: StackType): Option[(StackType, AbstractType)] = s match {
    case s: NonEmpty => Some((s.rest, s.top))
    case _ => None
  }
}

class NonEmpty(val top: AbstractType, val rest: StackType) extends StackType {
  override def toString = rest.toString + " " + top.toString

  def hasOccurrence(t: Variable) = top.hasOccurrence(t) || rest.hasOccurrence(t)
  def isMonomorphic = top.isMonomorphic || rest.isMonomorphic
  def isPolymorphic = top.isPolymorphic || rest.isPolymorphic

  def variables = top.variables ++ rest.variables
  def substitute(s: Substitution): StackType =
    rest.substitute(s).asInstanceOf[StackType] :+ top.substitute(s)

  def unifyWith(t: AbstractType, s: Substitution) = {
    val me = substitute(s)
    val (aRest :+ aTop) = me

    t.substitute(s) match {
      case bRest :+ bTop =>
        aTop.unifyWith(bTop, s).flatMap(s => aRest.unifyWith(bRest, s))
      case t: Remainder =>
        if (me.hasOccurrence(t)) None
        else Some(s.addBinding(t, me))
      case _ => None
    }
  }
}
