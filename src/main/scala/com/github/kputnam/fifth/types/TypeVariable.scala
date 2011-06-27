package com.github.kputnam.fifth.types

case class TypeVariable(id: Int) extends Type with Variable {
  val alphabet =
    lowerLatin

  def unifyWith(t: Type, s: Substitution) = {
    t.substitute(s).flatMap { he =>
      substitute(s) flatMap {
        case me: TypeVariable => he match {
          case he: TypeVariable =>
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