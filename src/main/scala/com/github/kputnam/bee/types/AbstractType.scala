package com.github.kputnam.bee.types

abstract class AbstractType {

  def quote: WordType =
    WordType(Remainder(0).rename(freeVariables),
             Remainder(0).rename(freeVariables) :+ this)

  // Value types (StringType, NumericType, etc) can be viewed as a nullary
  // function call that pushes a value onto that stack
  def asWord: WordType =
    WordType(Remainder(0).rename(freeVariables),
             Remainder(0).rename(freeVariables) :+ this)

  // True if the given variable occurs in this type expression
  def hasOccurrence(t: Variable): Boolean = freeVariables.contains(t)

  // Polymorphic types consist of at least one type variable
  def isPolymorphic: Boolean = freeVariables.nonEmpty
  def isMonomorphic: Boolean = freeVariables.isEmpty

  // Returns the set of all free variables in this type expression
  def freeVariables: Set[Variable]

  // Replaces bound variables in this type expression according to their
  // bindings in the given substitution (leaves free variables as-is)
  def substitute(s: Substitution): AbstractType

  // Creates a substitution that unifies this type expression with the other
  def unifyWith(t: AbstractType, s: Substitution): Option[Substitution]
  def unifyWith(t: AbstractType): Option[Substitution] = unifyWith(t, Substitution.empty)

  // Returns true if this is an "instance" of that
  def instanceOf(that: AbstractType): Boolean = throw new UnsupportedOperationException
  def equivalentTo(that: AbstractType): Boolean = this.instanceOf(that) && that.instanceOf(this)
  def subtypeOf(that: AbstractType): Boolean = throw new UnsupportedOperationException

  // Generate fresh variables for each of the given variables
  def rename(bound: Set[Variable]): this.type = {
    var allocated = (freeVariables | bound).map(_.id)
    var conflicts =  freeVariables & bound

    val substitution = conflicts.foldLeft(Substitution.empty) { (s, v) =>
      val id = Iterator.from(0).find(id => !allocated.contains(id)).get
      allocated += id

      v match {
        case _: TypeVariable  => s.addBinding(v, TypeVariable(id))
        case _: Remainder     => s.addBinding(v, Remainder(id))
      }
    }

    substitute(substitution).asInstanceOf[this.type]
  }
}
