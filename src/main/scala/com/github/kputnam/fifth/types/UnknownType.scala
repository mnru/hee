package com.github.kputnam.fifth.types

case class UnknownType(id: Int) extends Type {
//val alphabet = "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩαβγδεζηθικλμνξοπρςστυφχψω"
  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

  override def toString =
    if (id >= alphabet.length) id.toString
    else alphabet.slice(id, id + 1)

  def substitute(s: Substitution) =
    Some(s.getOrElse(this, this))

  def hasOccurrence(t: UnknownType) =
    id == t.id

  def occursIn(t: Type) =
    t.hasOccurrence(this)

  def unifyWith(t: Type, s: Substitution) = {
    t.substitute(s).flatMap { he =>
      substitute(s) flatMap {
        case me: UnknownType => he match {
          case he: UnknownType =>
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