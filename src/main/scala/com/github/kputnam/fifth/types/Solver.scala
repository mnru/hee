package com.github.kputnam.fifth.types

import tools.nsc.backend.icode.analysis.CopyPropagation.Unknown

/**
 * boolean:   true false
 * number:    0, 342.54, -3
 * string:    "abc xyz", ""
 * 
 * top:       all values
 * bottom:    no values
 *
 * boolean:   nullary, monomorphic
 * number:    nullary, monomorphic
 * string:    nullary, monomorphic
 * function:  a → b, polymorphic
 * record:    a × b, polymorphic
 *
 * polymorphic types
 * variant types
 * intersection types
 */

sealed abstract class Type {
  def substitute(s: Substitution): Type
  def hasOccurrence(t: UnknownType): Boolean
  def unifyWith(t: Type, s: Substitution): Option[Substitution]
}

sealed abstract class MonomorphicType extends Type {
  def substitute(s: Substitution) = this
  def hasOccurrence(t: UnknownType) = false
  def unifyWith(t: Type, s: Substitution) = {
    t.substitute(s) match {
      case he: UnknownType => Some(s.addBinding(he, this))
      case he =>
        if (he == this) Some(s)
        else None
    }
  }
}

case object StringType extends MonomorphicType
case object BooleanType extends MonomorphicType
case object NumericType extends MonomorphicType

case class StackType(head: List[Type]) extends Type {
  override def toString =
    head.mkString("[", " ", "]")

  def substitute(s: Substitution) =
    StackType(head.map(_.substitute(s)))

  def hasOccurrence(t: UnknownType) =
    head.exists(_.hasOccurrence(t))

  def unifyWith(t: Type, s: Substitution) = {
    val me = substitute(s)

    t.substitute(s) match {
      case he: UnknownType =>
        if (me.hasOccurrence(he)) None
        else Some(s.addBinding(he, me))
      case he: StackType =>
        if (me.head.size != he.head.size) None
        else me.head.zip(he.head).foldLeft(Some(s): Option[Substitution]) ((s, pair) =>
          s.flatMap(s => pair._1.unifyWith(pair._2, s)))
      case _ => None
    }
  }
}

object StackType {
  def apply(head: Type*): StackType = apply(head.toList)
}

case class WordType(input: Type, output: Type) extends Type {
  def substitute(s: Substitution) =
    WordType(input.substitute(s), output.substitute(s))

  def hasOccurrence(t: UnknownType) =
    input.hasOccurrence(t) || output.hasOccurrence(t)

  def unifyWith(t: Type, s: Substitution) = {
    val me = substitute(s)

    t.substitute(s) match {
      case he: UnknownType =>
        if (me.hasOccurrence(he)) None
        else Some(s.addBinding(he, me))
      case he: WordType =>
        me.input.unifyWith(he.input, s).flatMap(s => me.output.unifyWith(he.output, s))
      case _ => None
    }
  }
}

case class TupleType(fields: Map[String, Type]) extends Type {
  def substitute(s: Substitution) =
    TupleType(fields.map(p => (p._1, p._2.substitute(s))))

  def hasOccurrence(t: UnknownType) =
    fields.exists(p => p._2.hasOccurrence(t))

  def unifyWith(t: Type, s: Substitution) = {
    val me = substitute(s)

    t.substitute(s) match {
      case he: UnknownType =>
        if (me.hasOccurrence(he)) None
        else Some(s.addBinding(he, me))
      case he: TupleType =>
        if (me.fields.keys != he.fields.keys) None
        else me.fields.keys.foldLeft(Some(s): Option[Substitution]) ((s, key) =>
          s.flatMap(s => me.fields(key).unifyWith(he.fields(key), s)))
      case _ => None
    }
  }
}

object TupleType {
  def apply(fields: Pair[String, Type]*): TupleType = apply(Map(fields : _*))
}

case class VariantType(variants: List[Type]) extends Type {
  def substitute(s: Substitution) =
    VariantType(variants.map(_.substitute(s)))

  def hasOccurrence(t: UnknownType) =
    variants.exists(_.hasOccurrence(t))

  def unifyWith(t: Type, s: Substitution) = {
    val me = substitute(s)

    t.substitute(s) match {
      case he: UnknownType =>
        if (me.hasOccurrence(he)) None
        else Some(s.addBinding(he, me))
      case he: VariantType =>
        if (me.variants.size != he.variants.size) None
        else me.variants.zip(he.variants).foldLeft(Some(s): Option[Substitution]) ((s, pair) =>
          s.flatMap(s => pair._1.unifyWith(pair._2, s)))
      case _ => None
    }
  }
}

object VariantType {
  def apply(variants: Type*): VariantType = apply(variants.toList)
}

case class UnknownType(id: Int) extends Type {
//val alphabet = "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩΪΫάέήίΰαβγδεζηθικλμνξοπρςστυφχψωϊϋόύώ"
  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

  override def toString =
    if (id >= alphabet.length) id.toString
    else alphabet.slice(id, id + 1)

  def substitute(s: Substitution) =
    s.getOrElse(this, this)

  def hasOccurrence(t: UnknownType) =
    id == t.id

  def occursIn(t: Type) =
    t.hasOccurrence(this)

  def unifyWith(t: Type, s: Substitution) = {
    val he = t.substitute(s)

    substitute(s) match {
      case me: UnknownType => he match {
        case he: UnknownType =>
          if (me.id == he.id) Some(s)
          else Some(s.addBinding(me, he))
        case _ =>
          if (he.hasOccurrence(me)) None
          else Some(s.addBinding(me, he))
      }
      case me => me.unifyWith(he, s)
    }
  }
}

case class Substitution(s: Map[UnknownType, Type]) {
  def getOrElse(k: UnknownType, default: Type) =
    s.getOrElse(k, default)
  
  def addBinding(k: UnknownType, v: Type) = {
    val t = Substitution(Map(k -> v))
    Substitution(s.map(p => (p._1, p._2.substitute(t))) + (k -> v))
  }
}

object Notes {
  // a pop ::
  val tPop = WordType(StackType(UnknownType(0)),
                      StackType())

  // a dup :: a a
  val tDup = WordType(StackType(UnknownType(0)),
                      StackType(UnknownType(0), UnknownType(0)))

  // a id :: a
  val tId  = WordType(StackType(UnknownType(0)),
                      StackType(UnknownType(0)))

  // b a swap :: a b
  val tSwap  = WordType(StackType(UnknownType(0), UnknownType(1)),
                        StackType(UnknownType(1), UnknownType(0)))

  // (S -> T) (T -> U) compose :: (S -> U)
  val tCompose = WordType(StackType(WordType(UnknownType(0), UnknownType(1)),
                                    WordType(UnknownType(1), UnknownType(2))),
                          StackType(WordType(UnknownType(0), UnknownType(2))))

  // S (S -> T) apply :: T
  val tApply = WordType(StackType(UnknownType(0),
                                  WordType(UnknownType(0), UnknownType(1))),
                        StackType(UnknownType(1)))

  // a quote :: (S -> a)
  val tQuote = WordType(StackType(UnknownType(0)),
                        StackType(
                          WordType(UnknownType(1),
                                   StackType(UnknownType(0)))))
}