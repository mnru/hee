package com.github.kputnam.bcat.types

case class WordType(input: StackType, output: StackType) extends AbstractType {
  override def toString =
    "(" + input + " -> " + output + ")"

  def asWord = this

  def hasOccurrence(t: Variable) =
    input.hasOccurrence(t) || output.hasOccurrence(t)

  def isMonomorphic = input.isMonomorphic || output.isMonomorphic
  def isPolymorphic = input.isPolymorphic || output.isPolymorphic

  def variables = input.variables ++ output.variables

  def substitute(s: Substitution): WordType =
    WordType(input.substitute(s).asInstanceOf[StackType],
             output.substitute(s).asInstanceOf[StackType])

  def unifyWith(t: AbstractType, s: Substitution) = t match {
    case t: TypeVariable => Some(s.addBinding(t, substitute(s)))
    case _: WordType =>
      val he = t.substitute(s).asInstanceOf[WordType]
      val me =   substitute(s).asInstanceOf[WordType]

      me.input.unifyWith(he.input, s).flatMap(s => me.output.unifyWith(he.output, s))
  }

  def chainInto(t: WordType, s: Substitution) = {
    val he = t.substitute(s).asInstanceOf[WordType]
    val me =   substitute(s).asInstanceOf[WordType]

    me.output.unifyWith(he.input, s).map(s =>
      Pair(WordType(me.input.substitute(s).asInstanceOf[StackType],
                    he.output.substitute(s).asInstanceOf[StackType]), s))
  }
}
