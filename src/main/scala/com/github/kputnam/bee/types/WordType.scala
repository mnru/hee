package com.github.kputnam.bee.types

case class WordType(input: StackType, output: StackType) extends Type {
  override def toString =
    "(" + input + " → " + output + ")"

  override def asWord = this

  def freeVariables =
    input.freeVariables ++ output.freeVariables

  // Chain this word
  def >>(τ: WordType, s: Substitution = Substitution.empty) =
    s.unify(output, τ.input).map(_(τ.output))

  def substitute(s: Substitution) =
    WordType(input.substitute(s), output.substitute(s))
}
