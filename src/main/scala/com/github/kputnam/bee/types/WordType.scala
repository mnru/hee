package com.github.kputnam.bee.types

case class WordType(input: StackType, output: StackType) extends AbstractType {
  override def toString =
    "(" + input + " → " + output + ")"

  override def asWord = this

  def freeVariables =
    input.freeVariables ++ output.freeVariables

  def >>(t: WordType, s: Substitution = Substitution.empty) =
    s.unify(output, t.input).map(_(t.output))

  def substitute(s: Substitution) =
    WordType(input.substitute(s), output.substitute(s))
}
