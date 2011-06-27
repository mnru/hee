package com.github.kputnam.fifth.types

import annotation.tailrec

object StackType {
  def empty: StackType =
    RestNil

  def apply(elements: Type*): StackType =
    if (elements.nonEmpty)
      elements.last match {
        case rest: StackType =>
          elements.init.reverse.foldLeft(rest)((s, t) => t :: s)
        case _ =>
          elements.reverse.foldLeft(empty)((s, t) => t :: s)
      }
    else empty
}

sealed trait StackType extends Type {
  def isEmpty: Boolean

  def top: Type

  def rest: StackType

  def ::(top: Type): StackType =
    new ::(top, this)

  def reverse: StackType =
    reverseOnto(StackType.empty)

  @tailrec
  final def reverseOnto(suffix: StackType): StackType =
    if (isEmpty) suffix
    else rest.reverseOnto(top :: suffix)

  def :+(element: Type): StackType =
    if (isEmpty) element :: this
    else top :: (rest :+ element)

  @tailrec
  final def exists(f: Type => Boolean): Boolean =
    if (isEmpty) false
    else f(top) || rest.exists(f)

  @tailrec
  final def foldLeft[T](value: T)(f: (T, Type) => T): T =
    if (isEmpty) value
    else rest.foldLeft(f(value, top))(f)

  def size: Int =
    foldLeft(0)((s, t) => s + 1)

  def toList: List[Type] =
    foldLeft(List.empty[Type])((l, t) => t :: l).reverse

  def mkString(prefix: String, separator: String, suffix: String) = {
    val sb = new StringBuilder
    sb.append(prefix)

    if (!isEmpty) {
      reverse.foldLeft(sb)((sb, t) => sb.append(t).append(separator))
      sb.setLength(sb.length - separator.length)
    }

    sb.append(suffix)
    sb.toString
  }

  override def toString: String =
    mkString("[", ", ", "]")
}

case object RestNil extends StackType {
  def isEmpty =
    true

  def top: Nothing =
    throw new NoSuchElementException("top of empty stack")

  def rest: StackType =
    throw new UnsupportedOperationException("rest of empty stack")

  def hasOccurrence(t: Variable) =
    false

  def isMonomorphic =
    true

  def isPolymorphic =
    false

  def substitute(s: Substitution): Option[StackType] =
    Some(this)

  def unifyWith(t: Type, s: Substitution): Option[Substitution] =
    t.substitute(s).flatMap {
      case he: RestVariable => s.addBinding(he, this)
      case RestNil => Some(s)
      case _ => None
    }
}

case class RestVariable(id: Int) extends StackType with Variable {
  def isEmpty =
    false

  def top =
    this

  def rest =
    RestNil

  def unifyWith(t: Type, s: Substitution) = {
    t.substitute(s).flatMap { he =>
      substitute(s) flatMap {
        case me: RestVariable => he match {
          case he: RestVariable =>
            if (me.id == he.id) Some(s)
            else s.addBinding(me, he)
          case _ =>
            if (he.hasOccurrence(me)) None
            else s.addBinding(me, he)
        }
        case me => me.unifyWith(he, s)
      }
    }
  }
}

final case class ::(top: Type, rest: StackType) extends StackType {
  def isEmpty = false

  def hasOccurrence(t: Variable) =
    exists(_.hasOccurrence(t))

  def isMonomorphic =
    !isPolymorphic
  
  def isPolymorphic =
    exists(_.isPolymorphic)

  def substitute(s: Substitution): Option[StackType] =
    top.substitute(s).flatMap(top =>
      rest.substitute(s).map(rest =>
        top :: rest.asInstanceOf[StackType]))

  def unifyWith(t: Type, s: Substitution) =
    substitute(s).flatMap { me =>
      t.substitute(s).flatMap {
        case he: StackType if !he.isEmpty =>
          me.top.unifyWith(he.top, s).flatMap(s => me.rest.unifyWith(he.rest, s))
        case _ => None
      }
    }
}