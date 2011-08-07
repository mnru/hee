package com.github.kputnam.bee.types

object Substitution {
  def empty = Substitution(Map.empty[VariableLike, Type])
  def apply(bindings: Pair[VariableLike, Type]*): Substitution =
    Substitution(Map(bindings:_*))
}

case class Substitution(bindings: Map[VariableLike, Type]) {

  override def toString =
    "Substitution(" + (new StringBuilder /: bindings) {(s, binding) =>
      if (!s.isEmpty)
        s.append(", ")
      s.append(binding._2)
      s.append("/")
      s.append(binding._1)} + ")"

  def isEmpty =
    bindings.isEmpty

  def getOrElse(x: VariableLike, τ: Type) =
    bindings.getOrElse(x, τ)

  // Add new binding
  def +(pair: Pair[VariableLike, Type]): Substitution = {
    // Update existing bindings
    val single = Substitution(pair)
    Substitution(bindings.mapValues(single(_)) + pair)
  }

  // Applies this substitution to a type expression
  def apply(τ: Type): Type = {
    println(toString + ".apply(" + τ + ")")
    if (isEmpty) τ
    else τ.substitute(this)
  }

  // Creates a new substitution that unifies both type expressions
  def unify(τa: Type, τb: Type): Option[Substitution] = {
    println(toString + ".unify(" + τa + ", " + τb + ")")

    (this(τa), this(τb)) match {
      case (τa: MonomorphicType, τb: MonomorphicType) =>
        Some(this)

      case (Empty, Empty) =>
        Some(this)

      case (x: Variable, y: Variable) =>
        if (x.id == y.id) Some(this)
        else Some(this + (x -> y))

      case (x: Remainder, y: Remainder) =>
        if (x.id == y.id) Some(this)
        else Some(this + (x -> y))

      case (x: Variable, τ: Type) if !τ.isInstanceOf[StackType] =>
        if (x.occursIn(τ)) None
        else Some(this + (x -> τ))

      case (τ: Type, x: Variable) if !τ.isInstanceOf[StackType] =>
        if (x.occursIn(τ)) None
        else Some(this + (x -> τ))

      case (τa: WordType, τb: WordType) =>
        unify(τa.input, τb.input).flatMap(s =>
          s.unify(τa.output, τb.output))

      case (x: Remainder, τ: StackType) =>
        if (x.occursIn(τ)) None
        else Some(this + (x -> τ))

      case (τ: StackType, x: Remainder) =>
        if (x.occursIn(τ)) None
        else Some(this + (x -> τ))

      case (NonEmpty(τaT, τaR), NonEmpty(τbT, τbR)) =>
        unify(τaT, τbT).flatMap(s =>
          s.unify(τaR, τbR))

      case (_, _) => None
    }}
}
