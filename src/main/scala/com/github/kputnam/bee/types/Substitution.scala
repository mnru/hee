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

  def variables: Set[Variable] = bindings.keys.toSet

  def getOrElse(k: Variable, default: AbstractType) =
    bindings.getOrElse(k, default)

  def addBinding(k: Variable, v: AbstractType): Substitution =
    if (k == v)
      return this
    else // Substitute v for all occurrences of k in existing bindings
      Substitution(bindings.mapValues(v => v.substitute(Substitution(k -> v))) + (k -> v))
}
