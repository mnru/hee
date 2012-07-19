package com.github.kputnam.bee
package static

import types._

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

  def getOrElse(α: VariableLike, τ: Type) =
    bindings.getOrElse(α, τ)

  // Create new binding and update existing bindings
  def +(pair: Pair[VariableLike, Type]): Substitution = {
    val single = Substitution(pair)
    Substitution(bindings.mapValues(single(_)) + pair)
  }

  def \(α: VariableLike) =
    Substitution(bindings - α)

  /** Applies this substitution to a type expression */
  def apply(τ: Type): Type = {
    println(toString + ".apply(" + τ + ")")
    if (isEmpty) τ
    else τ.substitute(this)
  }

  /** Combine substitutions */
  def ∘(s: Substitution) = compose(s)
  def compose(s: Substitution) =
    new Substitution(bindings ++ s.bindings)

  /** Creates a new substitution that unifies both type expressions */
  def unify(τa: Type, τb: Type): Option[Substitution] = {
    println(toString + ".unify(" + τa + ", " + τb + ")")

    (this(τa), this(τb)) match {
      case (τa: MonomorphicLike, τb: MonomorphicLike) =>
        if (τa == τb) Some(this) else None

      case (α: Variable, β: Variable) =>
        if (α.id == β.id) Some(this)
        else Some(this + (α -> β))

      case (α: Tail, β: Tail) =>
        if (α.id == β.id) Some(this)
        else Some(this + (α -> β))

      case (α: Variable, τ: Type) if !τ.isInstanceOf[StackType] =>
        if (α.occursIn(τ)) None
        else Some(this + (α -> τ))

      case (τ: Type, α: Variable) if !τ.isInstanceOf[StackType] =>
        if (α.occursIn(τ)) None
        else Some(this + (α -> τ))

      case (WordType(τIn, τOut), WordType(σIn, σOut)) =>
        unify(τIn, σIn).flatMap(s =>
          s.unify(τOut, σOut))

      case (α: Tail, τ: StackType) =>
        if (α.occursIn(τ)) None
        else Some(this + (α -> τ))

      case (τ: StackType, α: Tail) =>
        if (α.occursIn(τ)) None
        else Some(this + (α -> τ))

      case (NonEmpty(τTail, τHead), NonEmpty(σTail, σHead)) =>
        unify(τTail, σTail).flatMap(s =>
          s.unify(τHead, σHead))

      case (τ, UniversalType(α, σ)) =>
        unify(τ, σ)

      case (UniversalType(α, τ), σ) =>
        unify(τ, σ)

      case (_, _) => None
    }}
}
