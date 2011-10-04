package com.github.kputnam.bee.types

/**
 * τ := α
 *    | const
 *    | τ → τ
 *    | τ ⨯ τ
 */
abstract class Type {

  def quote: WordType =
    WordType(Remainder(0).rename(freeVariables),
             Remainder(0).rename(freeVariables) :+ this)

  // Value types (StringType, NumericType, etc) can be viewed as a nullary
  // function call that pushes a value onto that stack
  def asWord: WordType =
    WordType(Remainder(0).rename(freeVariables),
             Remainder(0).rename(freeVariables) :+ this)

  // True if the given variable occurs in this type expression
  def hasOccurrence(x: VariableLike): Boolean = freeVariables.contains(x)

  // Polymorphic types consist of at least one type variable
  def isPolymorphic: Boolean = freeVariables.nonEmpty
  def isMonomorphic: Boolean = freeVariables.isEmpty

  // Returns the set of all free variables in this type expression
  def freeVariables: Set[VariableLike]

  def substitute(s: Substitution): Type

  // Generate fresh variables for each of the given variables
  def rename(bound: Set[VariableLike]): this.type = {
    var allocated = (freeVariables | bound).map(_.id)
    val conflicts =  freeVariables & bound

    // Build a substitution of only Variable -> Variable
    val substitution = (Substitution.empty /: conflicts) { (s, x) =>
      val freshId = Iterator.from(0).find(id => !allocated.contains(id)).get
      allocated  += freshId

      x match {
        case _: Variable  => s + (x -> Variable(freshId))
        case _: Remainder => s + (x -> Remainder(freshId))
      }
    }

    // We can trust this cast because we only substituted VariableLike -> VariableLike
    substitution(this).asInstanceOf[this.type]
  }
}
