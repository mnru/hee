package com.github.kputnam.bee.types

object StackType {
  def empty: StackType = Empty

  def variable(id: Int): StackType = Remainder(id)

  /** StackType(Z Y X ... C B A).top = A */
  def apply(elements: AbstractType*): StackType =
    (empty /: elements)((stack, e) => stack :+ e)

  /** StackType(List(Z Y X ... C B A)).top = A */
  def apply(elements: List[AbstractType]): StackType =
    (empty /: elements)((stack, e) => stack :+ e)
}

sealed abstract class StackType extends AbstractType {
  def top: AbstractType
  def rest: StackType

  /** Push `top` on the stack (left and right-associative operators) */
  def ::(top: AbstractType): StackType = new NonEmpty(top, this)
  def :+(top: AbstractType): StackType = new NonEmpty(top, this)

  override
  def asWord = throw new UnsupportedOperationException
  def toWord = throw new UnsupportedOperationException

  def substitute(s: Substitution): StackType
}

object Remainder {
  def fromString(c: Char) =
    new Remainder(Variable.toInt(c))

  def fromString(s: String) =
    new Remainder(Variable.toInt(s))
}

case class Remainder(id: Int) extends StackType with Variable {
  def top = throw new NoSuchElementException("top of placeholder stack")
  def rest = throw new UnsupportedOperationException("rest of placeholder stack")

  def alphabet = Variable.upperGreek

  def substitute(s: Substitution): StackType =
    s.getOrElse(this, this) match {
      case t: StackType => t
      case t => throw new UnsupportedOperationException(toString + " resolved to " + t)
    }
}

case object Empty extends StackType {
  def top = throw new NoSuchElementException("top of empty stack")
  def rest = throw new UnsupportedOperationException("rest of empty stack")

  override def toString = "∅"

  override def ::(top: AbstractType): StackType = top match {
    case t: Remainder => t
    case t => new NonEmpty(top, this)
  }

  override def :+(top: AbstractType): StackType = top match {
    case t: Remainder => t
    case t => new NonEmpty(top, this)
  }

  def freeVariables = Set.empty

  def substitute(s: Substitution) = this
}

case class NonEmpty(val top: AbstractType, val rest: StackType) extends StackType {
  override def toString = rest.toString + " " + top.toString

  def freeVariables = top.freeVariables ++ rest.freeVariables

  def substitute(s: Substitution) =
    new NonEmpty(top.substitute(s), rest.substitute(s))
}

/** Pattern matching object
  *   val (rest :: top) = stack
  *   case rest :: top => ...
  */
object :: {
  def apply(top: AbstractType, rest: StackType) =
    new NonEmpty(top, rest)

  def unapply(s: StackType) = s match {
    case s: NonEmpty => Some((s.top, s.rest))
    case _ => None
  }
}

/** Pattern matching object
  *   val (top :+ rest) = stack
  *   case top :+ rest => ...
  */
object :+ {
  def apply(rest: StackType, top: AbstractType) =
    new NonEmpty(top, rest)

  def unapply(s: StackType) = s match {
    case s: NonEmpty => Some((s.rest, s.top))
    case _ => None
  }
}
