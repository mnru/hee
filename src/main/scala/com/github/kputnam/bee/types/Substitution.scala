package com.github.kputnam.bee.types

object Substitution {
  def empty = Substitution(Map.empty[Variable, AbstractType])
  def apply(bindings: Pair[Variable, AbstractType]*): Substitution =
    Substitution(Map(bindings:_*))
}

case class Substitution(bindings: Map[Variable, AbstractType]) {

  override def toString =
    "Substitution(" + (new StringBuilder /: bindings) {(s, binding) =>
      if (!s.isEmpty)
        s.append(", ")
      s.append(binding._2)
      s.append("/")
      s.append(binding._1)} + ")"

  def isEmpty =
    bindings.isEmpty

  def domain: Set[Variable] =
    bindings.keys.toSet

  def range: Set[AbstractType] =
    bindings.values.toSet

  def getOrElse(k: Variable, default: AbstractType) =
    bindings.getOrElse(k, default)

  def addBinding(k: Variable, v: AbstractType): Substitution =
    if (k == v) this
    else {
      // Update existing bindings
      val single = Substitution(k -> v)
      Substitution(bindings.mapValues(single(_)) + (k -> v))
    }

  // Applies this substitution to a type expression
  def apply(t: AbstractType): AbstractType = {
    println(toString + ".apply(" + t + ")")

    if (isEmpty) t
    else t.substitute(this)
  }

  // Creates a new substitution that unifies both type expressions
  def unify(a: AbstractType, b: AbstractType): Option[Substitution] = {
    println(toString + ".unify(" + a + ", " + b + ")")
    val a_ = this(a)
    val b_ = this(b)

    (a_, b_) match {
      case (a: MonomorphicType, b: MonomorphicType) =>
        Some(this)

      case (Empty, Empty) =>
        Some(this)

      case (a: TypeVariable, b: AbstractType) if !b.isInstanceOf[StackType] =>
        if (a.occursIn(b)) None
        else Some(addBinding(a, b))

      case (a: AbstractType, b: TypeVariable) if !a.isInstanceOf[StackType] =>
        if (b.occursIn(a)) None
        else Some(addBinding(b, a))

      case (a: WordType, b: WordType) =>
        unify(a.input, b.input).flatMap(s =>
          s.unify(a.output, b.output))

      case (a: Remainder, b: StackType) =>
        Some(addBinding(a, b))

      case (a: StackType, b: Remainder) =>
        Some(addBinding(b, a))

      case (NonEmpty(aTop, aRest), NonEmpty(bTop, bRest)) =>
        unify(aTop, bTop).flatMap(s =>
          s.unify(aRest, bRest))

      case (_, _) => None
    }}
}
