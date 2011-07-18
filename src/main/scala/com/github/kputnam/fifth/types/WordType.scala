package com.github.kputnam.fifth.types

import com.github.kputnam.fifth.misc.union._

case class WordType(input: StackType, output: StackType) extends Type {
  override def toString =
    "(" + input + " -> " + output + ")"

  def hasOccurrence(t: Variable) =
    input.hasOccurrence(t) || output.hasOccurrence(t)

  def isMonomorphic =
    !isPolymorphic

  def isPolymorphic =
    input.isPolymorphic || output.isPolymorphic

  def variables =
    input.variables ++ output.variables

  def substitute(s: Substitution): Option[WordType] =
    input.substitute(s).flatMap(in =>
      output.substitute(s).map(out => WordType(in.asInstanceOf[StackType], out.asInstanceOf[StackType])))

  def unifyWith(t: Type, s: Substitution) = {
    substitute(s).flatMap { me =>
      t.substitute(s).flatMap {
        case he: TypeVariable =>
          if (me.hasOccurrence(he)) None
          else s.addBinding(he, me)
        case he: WordType =>
          me.input.unifyWith(he.input, s).flatMap(s => me.output.unifyWith(he.output, s))
        case _ => None
      }
    }
  }
}