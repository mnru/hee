package com.github.kputnam.bee
package types

import static._

/**
 * Words are functions that map stacks to stacks. Normally input and output
 * are StackType, but we also need to support universally and existentially
 * quantified StackTypes.
 */
case class WordType(input: Type, output: Type) extends Type {

  override def toString =
    "(" + input + " → " + output + ")"

  override def asWord = this

  def freeVariables =
    input.freeVariables ++ output.freeVariables

  /** Chain this word to the given word **/
  def apply(τ: WordType, s: Substitution = Substitution.empty) =
    s.unify(output, τ.input).map(_(τ.output))

  def compose(τ: WordType, s: Substitution = Substitution.empty) =
    s.unify(output, τ.input).map(_(WordType(input, τ.output)))

  def substitute(s: Substitution) =
    WordType(input.substitute(s), output.substitute(s))

  override def skolemize =
    WordType(input.skolemize, output.skolemize)

}
