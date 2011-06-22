package com.github.kputnam.fifth.types

object StackType {
  def apply(head: Type*): StackType = apply(head.toList)
}

case class StackType(head: List[Type]) extends Type {
  override def toString =
    head.mkString("[", " ", "]")

  def hasOccurrence(t: UnknownType) =
    head.exists(_.hasOccurrence(t))

  def substitute(s: Substitution): Option[StackType] =
    head.foldLeft(Some(List.empty): Option[List[Type]]) ((ts, t) =>
      ts.flatMap(ts => t.substitute(s).map(t => ts :+ t))
    ).map(StackType(_))

  def unifyWith(t: Type, s: Substitution) = {
    substitute(s).flatMap { me =>
      t.substitute(s).flatMap {
        case he: UnknownType =>
          if (me.hasOccurrence(he)) None
          else s.addBinding(he, me)
        case he: StackType =>
          if (me.head.size != he.head.size) None
          else me.head.zip(he.head).foldLeft(Some(s): Option[Substitution]) ((s, pair) =>
            s.flatMap(s => pair._1.unifyWith(pair._2, s)))
        case _ => None
      }
    }
  }
}