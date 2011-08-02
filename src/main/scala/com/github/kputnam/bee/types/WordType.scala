package com.github.kputnam.bee.types

case class WordType(input: StackType, output: StackType) extends AbstractType {
  override def toString =
    "(" + input + " → " + output + ")"

  override def asWord = this

  def freeVariables =
    input.freeVariables ++ output.freeVariables

  def substitute(s: Substitution): WordType =
    WordType(input.substitute(s).asInstanceOf[StackType],
             output.substitute(s).asInstanceOf[StackType])

  /** Unifies corresponding inputs and outputs */
  def unifyWith(t: AbstractType, s: Substitution) = t match {
    case t: TypeVariable => Some(s.addBinding(t, substitute(s)))
    case _: WordType =>
      val me =   substitute(s).asInstanceOf[WordType]
      val he = t.substitute(s).asInstanceOf[WordType]

      me.input.unifyWith(he.input, s).flatMap(s => me.output.unifyWith(he.output, s))
    case _ => None
  }

  /** Unifies one word's output with the other's input */
  def chainInto(t: WordType, s: Substitution) = {
    val he = t.substitute(s).asInstanceOf[WordType]
    val me =   substitute(s).asInstanceOf[WordType]

    me.output.unifyWith(he.input, s).map(s =>
      Pair(WordType(me.input.substitute(s).asInstanceOf[StackType],
                    he.output.substitute(s).asInstanceOf[StackType]), s))
  }
}
