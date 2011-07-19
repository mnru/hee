package com.github.kputnam.bcat.types

case class WordType(input: StackType, output: StackType) extends AbstractType {
  override def toString =
    "(" + input + " -> " + output + ")"

  def hasOccurrence(t: Variable) =
    input.hasOccurrence(t) || output.hasOccurrence(t)

  def isMonomorphic = input.isMonomorphic || output.isMonomorphic
  def isPolymorphic = input.isPolymorphic || output.isPolymorphic

  def variables = input.variables ++ output.variables

  def substitute(s: Substitution): WordType =
    WordType(input.substitute(s).asInstanceOf[StackType],
             output.substitute(s).asInstanceOf[StackType])

  def unifyWith(t: AbstractType, s: Substitution) = substitute(s) match {
    case m: WordType => t.substitute(s) match {
      case t: WordType =>
        m.input.unifyWith(t.input, s).flatMap(s => m.output.unifyWith(t.output, s))
      case _ => None
    }
    case _ => None
  }
}
