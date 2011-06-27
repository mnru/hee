package com.github.kputnam.fifth.types

object TupleType {
  def apply(fields: Pair[String, Type]*): TupleType = apply(Map(fields : _*))
}

case class TupleType(fields: Map[String, Type]) extends Type {
  def hasOccurrence(t: Variable) =
    fields.exists(p => p._2.hasOccurrence(t))

  def isMonomorphic =
    !isPolymorphic

  def isPolymorphic =
    fields.exists(p => p._2.isPolymorphic)

  def substitute(s: Substitution): Option[TupleType] =
    fields.foldLeft(Some(Map.empty): Option[Map[String, Type]]) ((ts, t) =>
      ts.flatMap(ts => t._2.substitute(s).map(s => ts + (t._1 -> s)))
    ).map(TupleType(_))

  def unifyWith(t: Type, s: Substitution) = {
    substitute(s).flatMap { me =>
      t.substitute(s) match {
        case he: TypeVariable =>
          if (me.hasOccurrence(he)) None
          else s.addBinding(he, me)
        case he: TupleType =>
          if (me.fields.keys != he.fields.keys) None
          else me.fields.keys.foldLeft(Some(s): Option[Substitution]) ((s, key) =>
            s.flatMap(s => me.fields(key).unifyWith(he.fields(key), s)))
        case _ => None
      }
    }
  }
}