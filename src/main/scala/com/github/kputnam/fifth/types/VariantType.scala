package com.github.kputnam.fifth.types

object VariantType {
  def apply(variants: Type*): VariantType = apply(variants.toList)
}

case class VariantType(variants: List[Type]) extends Type {
  def hasOccurrence(t: Variable) =
    variants.exists(_.hasOccurrence(t))

  def isMonomorphic =
    !isPolymorphic

  def isPolymorphic =
    variants.exists(_.isPolymorphic)

  def substitute(s: Substitution) =
    variants.foldLeft(Some(List.empty): Option[List[Type]]) ((ts, t) =>
      ts.flatMap(ts => t.substitute(s).map(t => ts :+ t))
    ).map(VariantType(_))

  def unifyWith(t: Type, s: Substitution) = {
    substitute(s).flatMap{ me =>
      t.substitute(s) match {
        case he: TypeVariable =>
          if (me.hasOccurrence(he)) None
          else s.addBinding(he, me)
        case he: VariantType =>
          if (me.variants.size != he.variants.size) None
          else me.variants.zip(he.variants).foldLeft(Some(s): Option[Substitution]) ((s, pair) =>
            s.flatMap(s => pair._1.unifyWith(pair._2, s)))
        case _ => None
      }
    }
  }
}