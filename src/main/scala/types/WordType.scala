package com.github.kputnam.bee.types

/**
 * Words are functions that map stacks to stacks. Normally input and output
 * are StackType, but we also need to support universally and existentially
 * quantified StackTypes.
 */
object WordType {
  type or[A, B] = Either[A, B]

  implicit def l[T](τ: T) = Left(τ)
  implicit def r[T](τ: T) = Right(τ)
}

import WordType._

case class WordType[I <% StackType or QuantifiedType,
                    O <% StackType or QuantifiedType](in: I, out: O) extends Type {

  val input  = in.asInstanceOf[Type]
  val output = out.asInstanceOf[Type]

  override def toString =
    "(" + input + " → " + output + ")"

  override def asWord = this

  def freeVariables =
    input.freeVariables ++ output.freeVariables

  /** Chain this word to the given word **/
  def compose(τ: WordType[I, O], s: Substitution = Substitution.empty) =
    s.unify(output, τ.input).map(_(τ.output))

  def substitute(s: Substitution) =
    WordType(input.substitute(s).asInstanceOf[I],
             output.substitute(s).asInstanceOf[O])

}
