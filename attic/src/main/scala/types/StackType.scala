package com.github.kputnam.bee
package types

import static._

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

abstract class StackType extends Type {
  def top: Type
  def rest: StackType

  /** Push `top` on the stack (left- and right-associative operators) */
  def ::(top: Type): StackType = NonEmpty(top, this)
  def :+(top: Type): StackType = NonEmpty(top, this)

  override
  def asWord: WordType = throw new UnsupportedOperationException
  def toWord: WordType = throw new UnsupportedOperationException

  def substitute(s: Substitution): StackType
}

object Tail {
  def fromName(c: Char) =
    Tail(VariableLike.toInt(c))

  def fromName(s: String) =
    Tail(VariableLike.toInt(s))
}

/**
 * Represents the remainder (bottom) of the stack as a variable. For instance
 * the type expression Tail.fromName('A') :+ NumericType :+ NumericType means
 * there two numbers sit at the top above some unknown stack A.
 *
 * This is only intended to occur as the last element in any stack. Hence the
 * expression Empty :+ Tail.fromName('A') is not well-formed. There is no means
 * to express that some middle portion of a stack in unknown.
 */
case class Tail(id: Int) extends StackType with VariableLike {
  def top  = throw new NoSuchElementException("top of placeholder stack")
  def rest = throw new UnsupportedOperationException("rest of placeholder stack")

  def alphabet =
    VariableLike.upperGreek

  def substitute(s: Substitution): StackType = s.getOrElse(this, this) match {
    case τ: StackType => τ
    case τ => throw new UnsupportedOperationException(
      "stack type variable " + toString + " resolved to non-stack type " + τ)
  }

  override def skolemize =
    SkolemizedTail(this)
}

case object Empty extends StackType with MonomorphicLike {
  def top  = throw new NoSuchElementException("top of empty stack")
  def rest = throw new UnsupportedOperationException("rest of empty stack")

  override def toString = "∅"

  override def ::(top: Type): StackType = top match {
    case x: Tail => x
    case τ => NonEmpty(top, this)
  }

  override def :+(top: Type): StackType = top match {
    case x: Tail => x
    case τ => NonEmpty(top, this)
  }
}

case class NonEmpty(val top: Type, val rest: StackType) extends StackType {
  require(!top.isInstanceOf[Tail], top + " may only at the bottom of the stack")

  override def toString =
    rest.toString + " " + top.toString

  def freeVariables =
    top.freeVariables ++ rest.freeVariables

  def substitute(s: Substitution) =
    NonEmpty(top.substitute(s), rest.substitute(s))

  override def skolemize =
    NonEmpty(top.skolemize, rest.skolemize.asInstanceOf[StackType])
}

/**
 * Pattern matching object
 *   val (rest :: top) = stack
 *   case rest :: top => ...
 */
object :: {
  def apply(top: Type, rest: StackType) =
    NonEmpty(top, rest)

  def unapply(τ: StackType) = τ match {
    case τ: NonEmpty => Some((τ.top, τ.rest))
    case _ => None
  }
}

/**
 * Pattern matching object
 *   val (top :+ rest) = stack
 *   case top :+ rest => ...
 */
object :+ {
  def apply(rest: StackType, top: Type) =
    NonEmpty(top, rest)

  def unapply(τ: StackType) = τ match {
    case τ: NonEmpty => Some((τ.rest, τ.top))
    case _ => None
  }
}
