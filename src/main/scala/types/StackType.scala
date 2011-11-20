package com.github.kputnam.bee.types

import com.github.kputnam.bee.static._

object StackType {
  def empty: StackType = Empty

  def variable(id: Int): StackType = Tail(id)

  /** StackType(Z Y X ... C B A).top = A */
  def apply(τs: Type*): StackType =
    (empty /: τs)((stack, τ) => stack :+ τ)

  /** StackType(List(Z Y X ... C B A)).top = A */
  def apply(τs: List[Type]): StackType =
    (empty /: τs)((stack, τ) => stack :+ τ)
}

sealed abstract class StackType extends Type {
  def top: Type
  def rest: StackType

  /** Push `top` on the stack (left and right-associative operators) */
  def ::(top: Type): StackType = new NonEmpty(top, this)
  def :+(top: Type): StackType = new NonEmpty(top, this)

  override
  def asWord = throw new UnsupportedOperationException
  def toWord = throw new UnsupportedOperationException

  def substitute(s: Substitution): StackType
}

object Tail {
  def fromName(c: Char) =
    new Tail(VariableLike.toInt(c))

  def fromName(s: String) =
    new Tail(VariableLike.toInt(s))
}

case class Tail(id: Int) extends StackType with VariableLike {
  def top = throw new NoSuchElementException("top of placeholder stack")
  def rest = throw new UnsupportedOperationException("rest of placeholder stack")

  def alphabet = VariableLike.upperGreek

  def substitute(s: Substitution): StackType =
    s.getOrElse(this, this) match {
      case τ: StackType => τ
      case τ => throw new UnsupportedOperationException(
        "stack type variable " + toString + " resolved to non-stack type " + τ)
    }
}

case object Empty extends StackType {
  def top = throw new NoSuchElementException("top of empty stack")
  def rest = throw new UnsupportedOperationException("rest of empty stack")

  override def toString = "∅"

  override def ::(top: Type): StackType = top match {
    case x: Tail => x
    case τ => new NonEmpty(top, this)
  }

  override def :+(top: Type): StackType = top match {
    case x: Tail => x
    case τ => new NonEmpty(top, this)
  }

  def freeVariables = Set.empty

  def substitute(s: Substitution) = this
}

case class NonEmpty(val top: Type, val rest: StackType) extends StackType {
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
  def apply(top: Type, rest: StackType) =
    new NonEmpty(top, rest)

  def unapply(τ: StackType) = τ match {
    case τ: NonEmpty => Some((τ.top, τ.rest))
    case _ => None
  }
}

/** Pattern matching object
  *   val (top :+ rest) = stack
  *   case top :+ rest => ...
  */
object :+ {
  def apply(rest: StackType, top: Type) =
    new NonEmpty(top, rest)

  def unapply(τ: StackType) = τ match {
    case τ: NonEmpty => Some((τ.rest, τ.top))
    case _ => None
  }
}
