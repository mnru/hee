package com.github.kputnam.bee.types

abstract class AbstractType {

  def quote: WordType =
    WordType(Remainder(0).rename(variables),
             Remainder(0).rename(variables) :+ this)

  // Value types (StringType, NumericType, etc) can be viewed as a nullary
  // function call that pushes a value onto that stack
  def asWord: WordType =
    WordType(Remainder(0).rename(variables),
             Remainder(0).rename(variables) :+ this)

  // True if the given variable occurs in this type expression
  def hasOccurrence(t: Variable): Boolean

  // Polymorphic types consist of at least one type variable
  def isPolymorphic: Boolean
  def isMonomorphic: Boolean

  // Returns the set of all variables in this type expression
  def variables: Set[Variable]

  // Replaces variables in this type expression according to their bindings
  // in the given substitution (leaves unbound variables as-is)
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
    var allocated = (variables | bound).map(_.id)
    var conflicts =  variables & bound

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
