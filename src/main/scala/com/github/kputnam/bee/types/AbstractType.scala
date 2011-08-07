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

  def substitute(s: Substitution): AbstractType

  // Generate fresh variables for each of the given variables
  def rename(bound: Set[Variable]): this.type = {
    var allocated = (freeVariables | bound).map(_.id)
    val conflicts =  freeVariables & bound

    // Build a substitution of only Variable -> Variable
    val substitution = (Substitution.empty /: conflicts) { (s, v) =>
      val freshId = Iterator.from(0).find(id => !allocated.contains(id)).get
      allocated += freshId

      v match {
        case _: TypeVariable  => s.addBinding(v, TypeVariable(freshId))
        case _: Remainder     => s.addBinding(v, Remainder(freshId))
      }
    }

    // We can trust this cast because we only substituted Variable -> Variable
    substitution(this).asInstanceOf[this.type]
  }
}
