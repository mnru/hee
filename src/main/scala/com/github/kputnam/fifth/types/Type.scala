package com.github.kputnam.fifth.types

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

abstract class Type {
  def hasOccurrence(t: Variable): Boolean
  def isPolymorphic: Boolean
  def isMonomorphic: Boolean

  def substitute(s: Substitution): Option[Type]
  def unifyWith(t: Type, s: Substitution): Option[Substitution]
}

sealed abstract class MonomorphicType extends Type {
  def hasOccurrence(t: Variable) = false
  def isPolymorphic = false
  def isMonomorphic = true

  def substitute(s: Substitution) = Some(this)

  def unifyWith(t: Type, s: Substitution) =
    t.substitute(s) match {
      case Some(he: TypeVariable)    => s.addBinding(he, this)
      case Some(he) if (this == he) => Some(s)
      case _ => None
    }
}

case object StringType extends MonomorphicType
case object BooleanType extends MonomorphicType
case object NumericType extends MonomorphicType